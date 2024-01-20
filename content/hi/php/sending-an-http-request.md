---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध (request) एक प्रोत्साहन है जिसका उपयोग वेब सर्वर और ग्राहकों के बीच संचार स्थापित करने के लिए किया जाता है। प्रोग्रामर्स इसका उपयोग डाटा लाने, भेजने और अन्य सर्वर इंटरैक्शन को संभालने के लिए करते हैं। 

## कैसे करें:

PHP के साथ HTTP अनुरोध भेजने की एक साधारण उदाहरण देखें:

```PHP
<?php
$url = "https://api.example.com/posts";
$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
curl_close($ch);
var_dump($response);
?>
```
इस कोड स्निपेट का परिणामस्वरूप आपको `https://api.example.com/posts` के लिए सर्वर से प्राप्त प्रतिक्रिया प्राप्त होगी। 

## गहराई डाइव

- **ऐतिहासिक प्रक्टन**: HTTP अनुरोध वेब अनुप्रयोगों का मुख्य ठेका है सुनिश्चित करने के लिए कि डाटा सही तरीके से आदान-प्रदान किया जा सकता है। 
- **विकल्प**: PHP में, cURL के अलावा `file_get_contents()` और `fsockopen()` जैसी अन्य विधियाँ भी हैं जिन्हें HTTP अनुरोधों को संभालने के लिए उपयोग किया जा सकता है।
- **अमल में लाने का विवरण**: PHP रनटाइम उस cURL लाइब्रेरी का निर्माण करता है जिसे सिस्टम पर ऑपन-सोर्स प्रोजेक्ट की तरह इंस्टॉल किया जाता है। 

## भी देखें:

- [PHP cURL डॉक्युमेंटेशन](https://www.php.net/manual/en/book.curl.php)