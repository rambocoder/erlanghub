// Alias the jQuery Namespace
        $(function() {

        var timestamp = 0;
        var pullUrl = "/pull";

        // bind events for the messages area

        var EventsDisplay = {
        print: function(messages){
        $.each(messages, function(index, value) {
        // work with value
        msg = value.replace("\\", "");
        obj = JSON.parse(msg);

        $("#msgList").prepend('
        <li>' + obj.type + " : " + obj.repo.name + '</li>
        ');
        });

        },
        error: function(error){
        $("#msgList").prepend('
        <li><span class="error">' + error + '</span></li>
        ');
        }
        };

        $('#Events').bind({
        'pull': function() {
        $.ajax({
        url: pullUrl + "/?since=" + timestamp,
        dataType: "json",
        method: "GET",
        success: function(data) {
        if(data.timestamp) {
        timestamp = data.timestamp;
        if (data.timeout == "true")
        {
        // means that a timeout was trigered on the server
        $('#Events').triggerHandler("pull")
        }else
        {
        EventsDisplay.print(data.messages);
        $('#Events').triggerHandler("pull")
        }
        }
        else {
        EventsDisplay.error("Failed to pull due to this data: " + data);
        // $('#Events').triggerHandler("pull");
        }
        },
        error: function(jqXHR, textStatus, errorThrown) {
        chatDisplay.error("Failed to pull due to this status: " + textStatus + " and error: " + errorThrown);
        // $('#Events').triggerHandler("pull");
        }
        });
        setTimeout(function() { /* Do Nothing here */},1);
        }
        }).triggerHandler("pull"); // kick off the pull event after all the events are bound to the Events object


        });