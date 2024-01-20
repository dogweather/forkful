---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक डेटा एक्सचेंज फॉर्मेट है। प्रोग्रामर्स इसे वेब एप्लिकेशन में डेटा भेजने और पाने के लिए करते हैं क्योंकि यह हल्का, पढ़ने में आसान और भाषा-निरपेक्ष है।

## How to: (कैसे करें?)
### PHP में JSON Encode करना:
```php
<?php
$data = array('name' => 'रोहित', 'age' => 25, 'city' => 'दिल्ली');
$json_data = json_encode($data, JSON_UNESCAPED_UNICODE);
echo $json_data;
?>
```
सैंपल आउटपुट:
```json
{"name":"रोहित","age":25,"city":"दिल्ली"}
```

### PHP में JSON Decode करना:
```php
<?php
$json_data = '{"name":"रोहित","age":25,"city":"दिल्ली"}';
$array_data = json_decode($json_data, true);
print_r($array_data);
?>
```
सैंपल आउटपुट:
```php
Array
(
    [name] => रोहित
    [age] => 25
    [city] => दिल्ली
)
```

## Deep Dive (गहराई में जानकारी)
JSON का आविष्कार 2001 में Douglas Crockford ने किया था। XML के विपरीत, JSON तेज़ी से पार्स होता है और इसका उपयोग JavaScript के साथ स्वाभाविक रूप से होता है। PHP में `json_encode()` और `json_decode()` फ़ंक्शंस का इस्तेमाल कर JSON को आसानी से हैंडल किया जा सकता है। ये फ़ंक्शन्स PHP के `json` एक्सटेंशन का हिस्सा हैं जो PHP 5.2.0 से स्टैंडर्ड हैं। 

## See Also (और जानकारी)
- PHP के अधिकृत मैन्युअल में JSON (https://www.php.net/manual/en/book.json.php)
- json_encode() के ऑफिशियल डॉक्यूमेंटेशन (https://www.php.net/manual/en/function.json-encode.php)
- json_decode() के ऑफिशियल डॉक्यूमेंटेशन (https://www.php.net/manual/en/function.json-decode.php)
- W3Schools पर JSON ट्यूटोरियल (https://www.w3schools.com/js/js_json_intro.asp)
- MDN Web Docs पर JSON गाइड (https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)