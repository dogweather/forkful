---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases:
- hi/php/writing-a-text-file.md
date:                  2024-02-03T19:30:25.631493-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
PHP में टेक्स्ट फ़ाइल लिखना एक फ़ाइल बनाने या खोलने और उसमें सामग्री डालने की प्रक्रिया होती है। प्रोग्रामर इसे डेटा को संग्रहीत करने के लिए करते हैं, जैसे प्रयोगकर्ता द्वारा निर्मित सामग्री या लॉग, प्रोग्राम के जीवनचक्र से परे।

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
