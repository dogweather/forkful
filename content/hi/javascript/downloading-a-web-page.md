---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पेज डाउनलोड करना मतलब है कि आप एक सरवर से विशिष्ट वेबसाइट की कॉपी अपने कंप्यूटर पर सहेज रहे हैं। प्रोग्रामर इसे तब करते हैं जब वे एक वेब पेज से अनुष्ठानात्मक डाटा प्राप्त करना चाहते हैं, या वेब पेज की स्थिर कॉपी की आवश्यकता होती है।

## कैसे करें:
```Javascript
var https = require('https');
var fs = require('fs');

var options = {
  host: 'example.com',
  path: '/index.html'
};

var req = https.get(options, function(res) {
  var bodyChunks = [];
  
  res.on('data', function(chunk) {
    bodyChunks.push(chunk);
  }).on('end', function() {
    var body = Buffer.concat(bodyChunks);
    fs.writeFile('index.html', body, function(err) {
      if(err) 
        console.error(err);
      else
        console.log('Data Saved!');
    });
  })
});

req.on('error', function(e) {
  console.error('ERROR: ' + e.message);
});
```
सैंपल आउटपुट:
```
Data Saved!
```

## गहरी गोता खोना
वेब पेजों को डाउनलोड करना अभी भी मौजूदा वेब का महत्वपूर्ण हिस्सा है, जैसे योजनाबद्ध spidering/scraping/crawling के लिए। इससे उड़ाहरण स्वायत्त स्क्रिप्ट बना सकते हैं, जो डाटा एकत्र करते हैं और उसे महत्त्वपूर्ण तरीकों में प्रगत या विश्लेषण करते हैं। इसके विकल्प के रूप में, आप क्रोम या फ़ायरफ़ॉक्स जैसे ब्राउज़रों का उपयोग कर सकते हैं, जिनमें डेवलपर उपकरण बुइल्ट-इन होते हैं। यदि आपका केस इतना स्पेसिफिक नहीं है, तो आप APIs या अन्य धारणाओं का भी उपयोग कर सकते हैं। 

## अन्य जानकारी
- [HTTP.ClientRequest](https://nodejs.org/api/http.html#http_class_http_clientrequest)
- [HTTP.IncomingMessage](https://nodejs.org/api/http.html#http_class_http_incomingmessage)
- [Buffer.concat](https://nodejs.org/api/buffer.html#buffer_class_method_buffer_concat_list_totalength)
- [fs.writeFile](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)