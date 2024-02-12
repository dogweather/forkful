---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases:
- /hi/php/reading-a-text-file/
date:                  2024-01-20T17:55:08.424042-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

पाठ फाइल पढ़ने का मतलब है फाइल से डेटा निकालना। प्रोग्रामर्स इसे क्यों करते हैं? सेटिंग्स, डेटा एनालिसिस, लॉग फाइल्स जैसे कई कारणों से। 

## How to: (कैसे करें:)

PHP में टेक्स्ट फाइल पढ़ने के लिए कोड:

```PHP
<?php
// फाइल का नाम
$fileName = 'example.txt';

// fopen() फंक्शन से फाइल खोलें
$fileHandle = fopen($fileName, 'r');

// अगर फाइल खुल गई तो, पढ़ें
if ($fileHandle) {
    while (($line = fgets($fileHandle)) !== false) {
        // प्रत्येक लाइन आउटपुट करें
        echo $line;
    }
    
    // अंत में, फाइल बंद कर दें
    fclose($fileHandle);
} else {
    // फाइल नहीं खुली, एरर दिखाएं
    echo "फाइल खोलने में असमर्थ!";
}
?>
```

सैम्पल आउटपुट:

```
हैलो, यह पहली लाइन है।
यह दूसरी लाइन है।
...
```

## Deep Dive (गहराई से जानकारी):

PHP में टेक्स्ट फाइल को पढ़ने के लिए कई तरीके हैं, जैसे `file_get_contents()`, `file()`, और `fopen()`। `fopen()` सबसे ज्यादा नियंत्रण देता है, जैसे कि बाइनरी फाइलों को पढ़ना। हिस्टोरिकली, `fopen()` C प्रोग्रामिंग भाषा से आया है और PHP ने इसे अपनाया।

अल्टरनेटिव्स जैसे `file_get_contents()` एक फाइल का डेटा एक स्ट्रिंग के रूप मे लौटाता है, जबकि `file()` फाइल को एक एरे में परिवर्तित करता है, जहां प्रत्येक लाइन एक एरे एलिमेंट होती है। 

जब बड़ी फाइलों को पढना हो, `fopen()` का इस्तेमाल बेहतर है क्योंकि यह मेमोरी-इफिशिएंसी प्रदान करता है - यह फाइल से प्रत्येक लाइन को बुफर में रखता है और फिर प्रोसेस करता है।

## See Also (और जानकारी के लिए:)

- PHP मैनुअल में फाइल सिस्टम...[PHP: Filesystem - Manual](https://www.php.net/manual/en/ref.filesystem.php)
- w3schools पर पीएचपी फाइल हैंडलिंग गाइड...[PHP 5 File - w3schools.com](https://www.w3schools.com/php/php_file.asp)
