---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:25.631493-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: PHP `file_put_contents`,\
  \ `fopen` \u0915\u0947 \u0938\u093E\u0925 `fwrite`, \u0914\u0930 `fclose` \u091C\
  \u0948\u0938\u0947 \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u0947 \u092E\
  \u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u092B\u093C\u093E\u0907\u0932 \u0932\
  \u0947\u0916\u0928 \u0915\u093E \u0938\u094D\u0935\u093E\u092D\u093E\u0935\u093F\
  \u0915 \u0930\u0942\u092A \u0938\u0947 \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\
  \u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0939\u0948\u2026"
lastmod: '2024-03-13T22:44:52.510291-06:00'
model: gpt-4-0125-preview
summary: "PHP `file_put_contents`, `fopen` \u0915\u0947 \u0938\u093E\u0925 `fwrite`,\
  \ \u0914\u0930 `fclose` \u091C\u0948\u0938\u0947 \u092B\u093C\u0902\u0915\u094D\u0936\
  \u0928 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u092B\u093C\
  \u093E\u0907\u0932 \u0932\u0947\u0916\u0928 \u0915\u093E \u0938\u094D\u0935\u093E\
  \u092D\u093E\u0935\u093F\u0915 \u0930\u0942\u092A \u0938\u0947 \u0938\u092E\u0930\
  \u094D\u0925\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\
  \u0901 \u0939\u0948 \u0915\u0948\u0938\u0947 \u0909\u0928\u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0947\u0902."
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u0932\u093F\u0916\u0928\u093E"
weight: 24
---

## कैसे करें:
PHP `file_put_contents`, `fopen` के साथ `fwrite`, और `fclose` जैसे फ़ंक्शन के माध्यम से फ़ाइल लेखन का स्वाभाविक रूप से समर्थन करता है। यहाँ है कैसे उनका उपयोग करें:

### `file_put_contents` के साथ सरल लेखन:
यह फ़ंक्शन एक ही चरण में फ़ाइल में लिखने की प्रक्रिया को सरल बना देता है।
```php
$content = "Hello, world!";
file_put_contents("hello.txt", $content);
// जांचता है कि फ़ाइल सफलतापूर्वक लिखी गई है या नहीं
if (file_exists("hello.txt")) {
    echo "File सफलतापूर्वक बनाई गई!";
} else {
    echo "फ़ाइल बनाने में विफल।";
}
```

### `fopen`, `fwrite`, और `fclose` के साथ उन्नत लेखन:
फ़ाइल लेखन पर अधिक नियंत्रण के लिए, जैसे कि पाठ जोड़ना या अधिक त्रुटि संभालना, `fopen` का `fwrite` के साथ उपयोग करें।
```php
$file = fopen("hello.txt", "a"); // 'a' मोड जोड़ने के लिए, 'w' लिखने के लिए
if ($file) {
    fwrite($file, "\nऔर अधिक सामग्री जोड़ना।");
    fclose($file);
    echo "सामग्री सफलतापूर्वक जोड़ी गई!";
} else {
    echo "फ़ाइल खोलने में विफल।";
}
```

#### आउटपुट के लिए फ़ाइल को पढ़ना:
हमारी सामग्री की जांच के लिए:
```php
echo file_get_contents("hello.txt");
```
**नमूना आउटपुट:**
```
Hello, world!
और अधिक सामग्री जोड़ना।
```

### थर्ड-पार्टी लाइब्रेरियों का उपयोग:
अधिक जटिल फ़ाइल ऑपरेशंस के लिए, `League\Flysystem` जैसी लाइब्रेरीज का उपयोग फ़ाइल सिस्टम के ऊपर एक अमूर्तता परत के रूप में किया जा सकता है, लेकिन PHP के निर्मित फ़ंक्शंस अक्सर मूल फ़ाइल लेखन कार्यों के लिए पर्याप्त होते हैं। यहाँ `Flysystem` का अन्वेषण करने का एक संक्षिप्त उदाहरण है:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "Using Flysystem to write this.");
```
यह उदाहरण मानता है कि आपने `league/flysystem` को Composer के माध्यम से स्थापित किया है। थर्ड-पार्टी लाइब्रेरी अधिक जटिल फाइल हैंडलिंग को बहुत सरल बना सकती है, विशेषकर विभिन्न स्टोरेज सिस्टमों के साथ निर्बाध रूप से काम करते समय।
