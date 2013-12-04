var mkh_w = 50;
var mkh_h = 50;
var mkh_pp = 1; // plants part (w*h/1000 * pp)

$(document).ready(function () {
    mkh_w = getURLParameter('w', 50);
    mkh_h = getURLParameter('h', 50);
    console.log('WH:', mkh_w, mkh_h);
    var field = $('#field');
    $('#field').css('width', mkh_w*10 + 'px');
    $('#field').css('height', mkh_h*10 + 'px');
    
    for(var i=0; i < mkh_h; i++){
        field.append($('<div id="row'+parseInt(i)+'"></div>'));
        var row = $('#row'+i);
        for(var j=0; j < mkh_w; j++){
            row.append($('<div class="cellr" id="c_'+(i*mkh_w+parseInt(j))+'"> </div>'));
        }
    }
    
    //init_grid();
    WebSocketTest1();  
});

function getURLParameter(name, def) {
    return decodeURI(
        (RegExp(name + '=' + '(.+?)(&|$)').exec(location.search)||[,def])[1]
    );
}

function init_grid(){
    
    test_moving(1);
}

function move_creature(from, to){
    unmark_pos(from);
    mark_pos(to);
}

function unmark_pos(pos){
    $('#c_'+pos).css('background', '#fff');
}

function mark_pos(pos){
    $('#c_'+pos).css('background', '#e00');
}

function random_move(pos){
    var o = get_pos_rc(pos);
    var new_r = o.r;
    var rand = Math.floor((Math.random()*100)+1);
    if (o.r > 0 && o.r < mkh_h-1) {
        if (rand < 33) {
            new_r += 1 
        }else if (rand < 66) {
            new_r -= 1
        }
    }else if (o.r == 0) {
        if (rand < 50) {
            new_r += 1 
        }
    }else if (o.r == mkh_h-1) {
        if (rand < 50) {
            new_r -= 1 
        }
    }
    
    var new_c = o.c;
    rand = Math.floor((Math.random()*100)+1);
    if (o.c > 0 && o.c < mkh_w) {
        if (rand < 33) {
            new_c += 1 
        }else if (rand < 66) {
            new_c -= 1
        }
    }else if (o.c == 0) {
        if (rand < 50) {
            new_c += 1 
        }
    }else if (o.c == mkh_w-1) {
        if (rand < 50) {
            new_c -= 1 
        }
    }
    
    var new_pos = new_r*mkh_w + new_c;
    return new_pos;
}

function test_moving(pos){
    var npos = random_move(pos)
    move_creature(pos, npos);
    setTimeout(function(){ test_moving(npos); }, 1000);
}

function get_pos_rc(pos){
    return {
            'r': Math.floor(pos/mkh_w),
            'c': pos % mkh_w
    };
}

function place_plant(pos){
    $('#c_'+pos).css('background', '#0f0');
}

function place_rock(pos){
    $('#c_'+pos).css('background', '#000');
}
function place_water(pos){
    $('#c_'+pos).css('background', '#0cf');
}

function WebSocketTest()
{
    if ("WebSocket" in window)
    {
        console.log("WebSocket is supported by your Browser!");
        // Let us open a web socket
        var uri = location.hostname+(location.port ? ':'+location.port: '');
        console.log("URI:" + uri);
        var ws = new WebSocket("ws://"+uri+"/websocket");
        ws.onopen = function()
        {
            // Web Socket is connected, send data using send()
            ws.send("init map " + mkh_w + ":" +mkh_h + ":" +mkh_pp);
            console.log("Message is sent...");
        };
        ws.onmessage = function (evt) 
        { 
            var received_msg = evt.data;
            var obj = JSON.parse(received_msg);
            for(var i = 0; i<obj.length; i++){
                if(obj[i][0] == 'plant'){
                    place_plant(obj[i][1])
                }else{
                    move_creature(obj[i][1], obj[i][2])
                }
            }
            console.log("Message is received...", received_msg);
        };
        ws.onclose = function()
        { 
            // websocket is closed.
            console.log("Connection is closed..."); 
        };
    }
    else
    {
        // The browser doesn't support WebSocket
        alert("WebSocket NOT supported by your Browser!");
    }
}

function WebSocketTest1()
{
    if ("WebSocket" in window)
    {
        console.log("WebSocket is supported by your Browser!");
        // Let us open a web socket
        var uri = location.hostname+(location.port ? ':'+location.port: '');
        console.log("URI:" + uri);
        var ws = new WebSocket("ws://"+uri+"/ws1");
        ws.onopen = function()
        {
            // Web Socket is connected, send data using send()
            ws.send("init map " + mkh_w + ":" +mkh_h + ":" +mkh_pp);
            console.log("Message is sent...");
        };
        ws.onmessage = function (evt) 
        { 
            var received_msg = evt.data;
            var obj = JSON.parse(received_msg);
            for(var i = 0; i<obj.length; i++){
                if(obj[i][1] == 'plant'){
                    place_plant(obj[i][2])
                }else if(obj[i][1] == 'rock'){
                    place_rock(obj[i][2])
                }else if(obj[i][1] == 'water'){
                    place_water(obj[i][2])
                }else{ 
                    move_creature(obj[i][2], obj[i][3])
                }
            }
            console.log("Message is received...", received_msg);
        };
        ws.onclose = function()
        { 
            // websocket is closed.
            console.log("Connection is closed..."); 
        };
    }
    else
    {
        // The browser doesn't support WebSocket
        alert("WebSocket NOT supported by your Browser!");
    }
}
