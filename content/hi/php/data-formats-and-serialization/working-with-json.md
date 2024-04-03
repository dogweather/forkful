---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:26.789970-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: PHP \u092E\u0947\u0902\
  \ JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 `json_encode()` \u0914\u0930 `json_decode()` \u0915\u0940 \u092C\u0926\
  \u094C\u0932\u0924 \u0938\u0930\u0932 \u0939\u0948\u0964 \u0928\u0940\u091A\u0947\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\
  \u0902 \u091C\u094B \u0926\u093F\u0916\u093E\u0924\u0947 \u0939\u0948\u0902 \u0915\
  \u093F \u0915\u0948\u0938\u0947\u2026"
lastmod: '2024-03-13T22:44:52.515371-06:00'
model: gpt-4-0125-preview
summary: "PHP \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\
  \u0902\u0915\u094D\u0936\u0928\u094D\u0938 `json_encode()` \u0914\u0930 `json_decode()`\
  \ \u0915\u0940 \u092C\u0926\u094C\u0932\u0924 \u0938\u0930\u0932 \u0939\u0948\u0964\
  \ \u0928\u0940\u091A\u0947 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u090F\
  \ \u0917\u090F \u0939\u0948\u0902 \u091C\u094B \u0926\u093F\u0916\u093E\u0924\u0947\
  \ \u0939\u0948\u0902 \u0915\u093F \u0915\u0948\u0938\u0947 \u090F\u0915 PHP \u0910\
  \u0930\u0947 \u0915\u094B JSON \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u093E \u091C\u093E\u090F, \u0914\u0930 \u0907\
  \u0938\u0915\u0947 \u0935\u093F\u092A\u0930\u0940\u0924."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:
PHP में JSON के साथ काम करना बिल्ट-इन फंक्शन्स `json_encode()` और `json_decode()` की बदौलत सरल है। नीचे उदाहरण दिए गए हैं जो दिखाते हैं कि कैसे एक PHP ऐरे को JSON स्ट्रिंग में बदला जाए, और इसके विपरीत:

### एक PHP ऐरे को JSON स्ट्रिंग में एन्कोड करना
```php
// एक सहयोगी ऐरे परिभाषित करें
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// PHP ऐरे को JSON स्ट्रिंग में बदलें
$jsonString = json_encode($data);

// JSON स्ट्रिंग को आउटपुट करें
echo $jsonString;
```
**नमूना आउटपुट:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### एक JSON स्ट्रिंग को PHP ऐरे में डिकोड करना
```php
// JSON स्ट्रिंग
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// JSON स्ट्रिंग को PHP ऐरे में बदलें
$data = json_decode($jsonString, true);

// PHP ऐरे को आउटपुट करें
print_r($data);
```
**नमूना आउटपुट:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### एक तृतीय-पक्ष पुस्तकालय के साथ काम करना: GuzzleHttp
जटिल JSON और वेब अनुरोध हैंडलिंग के लिए, एक लोकप्रिय PHP पुस्तकालय GuzzleHttp है। यह HTTP अनुरोधों को सरल बनाता है और JSON डेटा के साथ आसानी से काम करता है।

**Composer के माध्यम से स्थापना:**
```
composer require guzzlehttp/guzzle
```

**उदाहरण अनुरोध:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// एक एपीआई के लिए एक अनुरोध भेजें जो JSON वापस करता है
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// JSON प्रतिक्रिया को PHP ऐरे में डिकोड करें
$data = json_decode($response->getBody(), true);

// डेटा को आउटपुट करें
print_r($data);
```

**मान लें कि API समान JSON डेटा वापस करता है:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
यह PHP का उपयोग JSON मैनिपुलेशन के लिए करने की आसानी को प्रदर्शित करता है, देशी फंक्शन्स और अधिक जटिल कार्यों के लिए GuzzleHttp जैसे शक्तिशाली पुस्तकालयों के साथ।
