$(function () {
    var MAX_MESSAGES = 1000;

    var messages = $('#messages');
    var msg = $('#msg');
    var send = $('#send-button');

    var nick;

    var socket;
    var socketOpened = false;

    function onMessage(event) {
         var receivedMessage = JSON.parse(event.data);
        console.log("received message", event.data, receivedMessage);
        var el = $("<div>");

        switch (receivedMessage.type) {
            case "list":
                var ul = $("<ul>");
                receivedMessage.nicks.forEach(function (nick) {
                    ul.append($("<li>").html(nick));
                });
                el.append(ul);
                break;

            case "nick":
                el.append($("<span>").addClass("nick").html(receivedMessage.old));
                el.append(" changed nick to ");
                el.append($("<span>").addClass("nick").html(receivedMessage.new));
                break;

            case "join":
                el.append($("<span>").addClass("nick").html(receivedMessage.nick));
                el.append(" joined");
                break;

            case "part":
                el.append($("<span>").addClass("nick").html(receivedMessage.nick));
                el.append(" left");
                break;

            case "msg":
            case "priv":
                el.append($("<span>").addClass("nick").html(receivedMessage.sender));
                el.append(": ");
                el.append($("<span>").addClass("msg").html(receivedMessage.msg));
                break;

            case "error":
                el.html(receivedMessage.msg);
                break;

            case "ping":
                if (socketOpened) {
                    socket.send(JSON.stringify({
                        type: "pong",
                    }));
                }
                el.html(""); // ping line is empty
                break;

            default:
                el.html(event.data);
        }

        el.addClass(receivedMessage.type).addClass("message");
        messages.append(el);

        // cleanup history
        var children = messages.children();
        for (var i = 0; i < children.length - MAX_MESSAGES; i++) {
            $(children[i]).remove();
        }

        // scroll to the bottom
        messages.animate({ scrollTop: messages[0].scrollHeight });
    }

    function sendMessage() {
        var msgText = msg.val().trim();
        if (msgText === "") { return; }

        var message, m;

        if ((m = msgText.match(/^\/priv\s+(\S+)\s+([^]*)$/))) {
            message ={
                "type": "priv",
                "nick": m[1],
                "msg": m[2],
            };
        } else if ((m = msgText.match(/^\/nick\s+(\S+)/))) {
            nick = m[1];
            message = {
                "type": "nick",
                "nick": nick,
            };
        } else if (msgText.match(/^\/(?:n|names|list)(?:\s|$)/)) {
            message = {
                "type": "list",
            };
        } else {
            message = {
                "type": "msg",
                "msg": msgText,
            };
        }

        console.log(msgText, message);
        if (socketOpened) {
            socket.send(JSON.stringify(message));
        }

        msg.val("");
        msg.focus();
    }

    function newSocket(delay) {
        delay = Math.min(delay || 1000, 60000);

        if (socketOpened) {
            return;
        }

        socket = new WebSocket("ws://localhost:3000/ws/");

        socket.onopen = function () {
            // Change nick if we set one
            if (nick) {
                socket.send(JSON.stringify({
                    "type": "nick",
                    "nick": nick,
                }));
            }

            // socket is opened now
            socketOpened = true;
        };

        socket.onmessage = onMessage;

        socket.onclose = function (event) {
            socketOpened = false;

            console.error("Socket closed");

            setTimeout(newSocket.bind(null, delay * 2), delay);
        };

        socket.onerror = function (event) {
            socket.close();
            socketOpened = false;
        };
    }

    // Open connection
    newSocket();

    // Register listeners
    send.click(sendMessage);
    msg.keypress(function(e) {
        if(e.which == 13) {
            sendMessage();
        }
    });
});
