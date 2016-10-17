tests["Data Available"] = responseBody.has("mbid");

if (responseBody.has("mbid")) {
    
    try {
        
        var data = JSON.parse(responseBody);
        var mbid = [];
        for (var i = 0; i < 50; i++) {
            mbid[i] = data.similartracks.track[i].mbid;
        }
    }
    
    catch(e){
        console.log(e);
    }
    
    for (var i = 0; i < 50; i++) {
        postman.setGlobalVariable("mbid"+i, mbid[i]);
    }
    
} else {
    
    for (var i = 0; i < 50; i++) {
        postman.setGlobalVariable("mbid"+i, 0);
    }
}

//console.log(mbid);