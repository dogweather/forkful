---
date: 2024-01-20 18:00:19.103139-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PHP \u092E\
  \u0947\u0902 HTTP request \u092D\u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F `cURL` \u092F\u093E `file_get_contents` \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939\u093E\u0901 `cURL` \u0938\u0947 POST request \u092D\u0947\
  \u091C\u0928\u0947 \u0915\u093E \u0909\u0926\u093E\u0939\u0930\u0923\u2026"
lastmod: '2024-03-13T22:44:52.472185-06:00'
model: gpt-4-1106-preview
summary: "PHP \u092E\u0947\u0902 HTTP request \u092D\u0947\u091C\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `cURL` \u092F\u093E `file_get_contents` \u0915\u093E \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\
  \u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 `cURL` \u0938\u0947 POST\
  \ request \u092D\u0947\u091C\u0928\u0947 \u0915\u093E \u0909\u0926\u093E\u0939\u0930\
  \u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

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
