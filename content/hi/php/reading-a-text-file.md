---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक टेक्स्ट फ़ाइल को पढ़ना का मतलब होता है डिस्क पर स्थित किसी फ़ाइल की सामग्री को अपने कोड में लोड करना। प्रोग्रामर्स इसे करते हैं ताकि वे फ़ाइल की जानकारी को प्रोग्राम में काम में ले सकें।

## कैसे करें:
PHP में टेक्स्ट फ़ाइल को पढ़ने के लिए, हम `file_get_contents()` या `fopen()` और `fread()` का उपयोग कर सकते हैं।

उदाहरण के तौर पर, `file_get_contents()` का उपयोग:
```PHP
<?php
  $file = 'example.txt'; // फ़ाइल का पथ
  $contents = file_get_contents($file);
  echo $contents; // फ़ाइल की सामग्री प्रिंट करें
?>
```

सैम्पल आउटपुट:
```
Hello, this is a sample text file.
```
## गहरी जांच
हम `fopen()` और `fread()` का उपयोग भी कर सकते हैं, जो 1980 के दशक में C प्रोग्रामिंग भाषा का हिस्सा बने थे और PHP में एडॉप्ट किए गए थे। वैकल्पिक तरीके, जैसे कि `file()` और `readfile()`, भी उपलब्ध हैं जो विशेष परिस्थितियों में उपयोगी हो सकते हैं। पाठ फ़ाइल को पढ़ने का कोई भी तरीका, फ़ाइल हैंडल (यानी, फ़ाइल का "संदर्भ") और ओपरेटिंग सिस्टम की सहायता के बिना पढ़ने की सतत यात्रा के दौरान घटनाओं से बचने के लिए, लॉकिंग का उपयोग करना चाहिए।

## देखने के लिए भी
अधिक जानकारी के लिए, [PHP मैनुअल](https://www.php.net/manual/en/ref.filesystem.php) देखें। यदि आप फ़ाइल्स और स्ट्रीम्स के बारे में गहराई से जानकारी प्राप्त करना चाहते हैं, तो [PHP: The Right Way](https://phptherightway.com/#pdo_extension) गाइड देखें।