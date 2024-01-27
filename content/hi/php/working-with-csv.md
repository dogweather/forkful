---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma-Separated Values) एक साधारण फाइल फॉर्मेट है जिसमे डेटा अल्पविराम से अलग किया जाता है। प्रोग्रामर्स CSV का उपयोग इसलिए करते हैं क्योंकि यह हल्का (lightweight), सहज पठनीय (human-readable) और आसानी से निर्यात (export) और आयात (import) करने योग्य फॉर्मेट है।

## How to: (कैसे करें:)
### CSV फाइल पढ़ना:
```php
<?php
$filename = 'data.csv';

if (($h = fopen("{$filename}", "r")) !== FALSE) {
  while (($data = fgetcsv($h, 1000, ",")) !== FALSE) {
    print_r($data);
  }
  fclose($h);
}
?>
```
### CSV फाइल लिखना:
```php
<?php
$list = array (
    array('नाम', 'उम्र', 'शहर'),
    array('राहुल', '25', 'मुंबई'),
    array('प्रिया', '22', 'बैंगलोर')
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```
सैंपल आउटपुट नहीं होता; पर CSV फाइल में ये डेटा सेव हो जाएगा।

## Deep Dive (गहराई से जानकारी):
CSV फाइल फॉर्मेट 1970 के दशक से उपयोग में आ रहे हैं। इसके विकल्प के रूप में XML और JSON जैसे फॉर्मेट हैं, जो अधिक जटिल डेटा संरचनाओं का समर्थन करते हैं। PHP में, fgetcsv() और fputcsv() फंक्शन्स का उपयोग करके CSV फाइल से डेटा पढ़ने और लिखने में मदद मिलती है। इन फंक्शन्स को हैंडल करना आसान है और ये डेटा विश्लेषण और आदान-प्रदान में काफी कारगर हैं।

## See Also (और जानकारी के लिए):
- PHP Manual on fgetcsv: [PHP: fgetcsv - Manual](https://www.php.net/manual/en/function.fgetcsv.php)
- PHP Manual on fputcsv: [PHP: fputcsv - Manual](https://www.php.net/manual/en/function.fputcsv.php)
- CSV संरचना की विस्तृत जानकारी: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- CSV और XML/JSON के बीच तुलनात्मक अध्ययन: [Comparing CSV and XML/JSON](https://www.csvjson.com/csv-vs-json)
