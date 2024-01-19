---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# स्वागत है, Java कोडर्स!

## क्या और क्यों?

HTTP अनुरोध भेजना का मतलब होता है किसी सर्वर पर जानकारी मांगना या भेजना। यहां हम सर्वर को डेटा भेज सकते हैं या डेटा प्राप्त कर सकते हैं, जैसे ज्ञान पत्रिका या मासिका में नई घटनाएँ या खबरें।

## कैसे:

एक example:

```Javascript
var http = require('http');

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'text/html'});
    res.end('Hello, World!');
}).listen(8080);
```

यह उदाहरण एक HTTP सर्वर बनाता है, जो 8080 पोर्ट पर सुनता है और "Hello, World!" उत्तर देता है।

## गहराई में:
पहले के вер्सन में XMLHTTPRequest API का उपयोग किया जाता था HTTP संवाद के लिए, लेकिन यह जटिल हो सकता था। आजकल हम वादा-आधारित Fetch API का उपयोग कर सकते हैं।

Fetch API में प्रतिस्पर्धा:

```Javascript
fetch('http://example.com/movies.json')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log('गलती:', error));
```

Fetch API का उपयोग करना ध्यान देने योग्य एक विकल्प है।

## आगे देखें:
जावा के Fetch और HTTP अनुरोध के बारे में अधिक जानने के लिए, यहां कुछ उपयोगी स्रोत हैं.

- MDN Web Docs [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)।
- HTTP के बारे में विस्तृत जानकारी के लिए [HTTP guide](https://developer.mozilla.org/en-US/docs/Web/HTTP)।