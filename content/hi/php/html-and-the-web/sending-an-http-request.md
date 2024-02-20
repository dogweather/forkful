---
date: 2024-01-20 18:00:19.103139-07:00
description: "HTTP request \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0902\u0917\u0928\
  \u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 PHP \u092E\
  \u0947\u0902 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0921\u0947\u091F\u093E \u0932\u0947\u0928\u0947, \u090F\u092A\
  \u0940\u0906\u0908 \u0915\u0949\u0932 \u0915\u0930\u0928\u0947, \u092F\u093E \u0926\
  \u0942\u0938\u0930\u0940 \u0938\u093E\u0907\u091F\u094D\u0938 \u0938\u0947 \u0907\
  \u0902\u091F\u0930\u0948\u0915\u094D\u091F \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: 2024-02-19 22:05:11.484126
model: gpt-4-1106-preview
summary: "HTTP request \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947\
  \ \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0902\u0917\u0928\u0947\
  \ \u0915\u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 PHP \u092E\u0947\
  \u0902 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0921\u0947\u091F\u093E \u0932\u0947\u0928\u0947, \u090F\u092A\
  \u0940\u0906\u0908 \u0915\u0949\u0932 \u0915\u0930\u0928\u0947, \u092F\u093E \u0926\
  \u0942\u0938\u0930\u0940 \u0938\u093E\u0907\u091F\u094D\u0938 \u0938\u0947 \u0907\
  \u0902\u091F\u0930\u0948\u0915\u094D\u091F \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP request वेब सर्वर से जानकारी मांगने का तरीका है। PHP में प्रोग्रामर्स इसे डेटा लेने, एपीआई कॉल करने, या दूसरी साइट्स से इंटरैक्ट करने के लिए करते हैं।

## How to: (कैसे करें:)
PHP में HTTP request भेजने के लिए `cURL` या `file_get_contents` का इस्तेमाल किया जाता है। यहाँ `cURL` से POST request भेजने का उदाहरण दिया गया है:

```PHP
<?php
$url = 'https://api.example.com/data';
$payload = [
    'key1' => 'value1',
    'key2' => 'value2'
];

$curl = curl_init($url);
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
curl_setopt($curl, CURLOPT_POST, true);
curl_setopt($curl, CURLOPT_POSTFIELDS, http_build_query($payload));
$response = curl_exec($curl);
curl_close($curl);

echo $response;
?>
```

यदि request सफल होती है, तो `echo $response;` सर्वर का response प्रिंट करेगा।

## Deep Dive (गहराई से जानिए):
HTTP requests का उपयोग पहले वेब फॉर्म और ब्राउजर लिंक्स के माध्यम से होता था। PHP में `cURL` एक शक्तिशाली library है जो 2000's की शुरुआत से उपलब्ध है। यह विभिन्न प्रोटोकॉल्स सपोर्ट करती है और विस्तारित फीचर्स देती है। `file_get_contents` सरल use-case के लिए उपयुक्त है, पर `cURL` ज्यादा flexibility और control देता है।

समय के साथ PHP ने नई functionalities add की हैं, जैसे कि `http` stream wrapper के साथ `file_get_contents` का उपयोग करना। `cURL` का उपयोग जटिल और गतिशील requests के लिए अभी भी प्राथमिकता है। इसके विपरीत, समकालीन frameworks जैसे कि `Guzzle` जो PHP के लिए HTTP client का काम करते हैं, `cURL` की complexities को आसानी से manage करते हैं।

## See Also (इसे भी देखें):
- PHP cURL Documentation: https://www.php.net/manual/en/book.curl.php
- PHP Streams: https://www.php.net/manual/en/book.stream.php
- GuzzleHTTP Documentation: http://docs.guzzlephp.org/en/stable/
- PHP The Right Way (HTTP Requests): https://phptherightway.com/#http_requests
