---
title:                "PHP: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज को डाउनलोड करने का एक प्रमुख कारण है कि हम उस पेज पर उपलब्ध सामग्री का उपयोग ऑफलाइन मोड में कर सकते हैं, जिससे हमारे इंटरनेट संपर्क का दबाव कम होता है और हम उस पेज को पहुंचने में देरी भी नहीं होती।

## कैसे करें

वेब पेज को डाउनलोड करने के लिए हम `file_get_contents()` फ़ंक्शन का उपयोग कर सकते हैं, जिसे PHP में अत्यधिक सरलता से वेब पेज को डाउनलोड करने के लिए इस्तेमाल किया जाता है। यह फ़ंक्शन एक URL पर से सामग्री ग्रहण करता है और उसे एक स्ट्रिंग के रूप में वापस देता है।

```PHP
<?php
    $url = "https://www.example.com";
    $webpage = file_get_contents($url); // वेब पेज सामग्री डाउनलोड करना
    echo $webpage; // सामग्री मैं प्रिंट करें
?>
```

Output:

```HTML
<!DOCTYPE html>
<html>
    <head>
        <title>Example Page</title>
    </head>
    <body>
        <h1>Hello World!</h1>
        <p>This is an example webpage.</p>
    </body>
</html>
```

## गहराई में जाएं

वेब पेज को डाउनलोड करने के अलावा, हम वेब पेज के साथ अन्य इनपुट पैरामीटर और हेडर जानकारियां भी डाउनलोड कर सकते हैं। हम भी `stream_context_create()` और `file_get_contents()` फ़ंक्शन का उपयोग करके सामग्री को डाउनलोड करने के दौरान अलग सेटिंग्स को कन्फ़िगर कर सकते हैं। यह हमें वेब पेज से मीडिया फ़ाइल्स भी डाउनलोड करने की अनुमति देता है।

## देखें भी

- [PHP.net में `file_get_contents()` फ़ंक्शन डॉक्यूमेंटेशन](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP.net में `stream_context_create()` फ़ंक्शन डॉक्यूमेंटेशन](https://www.php.net/manual/en