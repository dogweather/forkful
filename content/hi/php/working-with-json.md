---
title:                "Json के साथ काम करना"
html_title:           "PHP: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON कोडिंग में लोग उसका उपयोग इसलिए करते हैं क्योंकि यह एक लोकप्रिय एवं सुरक्षित प्रारूप है जो डेटा को आसानी से संगठित करता है।

## कैसे करें

जब हम अपने PHP स्क्रिप्ट में JSON कोड का उपयोग करते हैं, तो हम उसे इस प्रकार से पारस्परिक विश्लेषण कर सकते हैं:

```PHP
$json = '{"name":"John", "age":30, "city":"New York"}'; // JSON स्ट्रिंग
$data = json_decode($json, true); // यह डेटा असोसियटिव एरे में परिवर्तित हो जाएगा
echo $data['name']; // John आउटपुट
echo $data['age']; // 30 आउटपुट
```

जोड़ सकते हैं:

```PHP
$data = ['name' => 'John', 'age' => 30, 'city' => 'New York']; // एरे
$json = json_encode($data); // इसे JSON स्ट्रिंग में परिवर्तित करें
echo $json; // {"name":"John","age":30,"city":"New York"} आउटपुट
```

इस तरह से, हम PHP में JSON कोड का उपयोग कर सकते हैं।

## गहराई में

JSON पैरसर पैकेज PHP में स्थापित है, इसलिए हम उसे कोड से आसानी से अपनी PHP एप्लीकेशन में शामिल कर सकते हैं। JSON कोडिंग के लिए यह एक ब्रिलियेंट और सरल विकल्प है।

## देखें भी

- [PHP में JSON कोडिंग का आसान गाइड](https://www.php.net/manual/en/book.json.php)
- [JSON.org में PHP](https://www.json.org/php.html)
- [PHP में JSON परिवर्तन](https://www.php.net/manual/en/json.requirements.php)