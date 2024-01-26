---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:41:24.768884-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

PHP में अस्थायी फ़ाइल बनाना डेटा को कुछ समय के लिए संग्रहीत करने की तकनीक है। प्रोग्रामर इसे डेटा प्रोसेसिंग, कैशिंग, या बड़ी फाइलों को शेयर करने में अस्थायी स्थान के रूप में इस्तेमाल करते हैं।

## How to: (कैसे करें:)

PHP में अस्थायी फ़ाइल बनाना बहुत सरल है। नीचे दिए कोड का उपयोग करके आप आसानी से एक अस्थायी फ़ाइल बना सकते हैं:

```PHP
<?php
$temp_file = tmpfile();
fwrite($temp_file, "यह एक अस्थायी सन्देश है।");
// कोड यहाँ ...

// फ़ाइल का उपयोग करने के बाद, इसे बंद करें:
fclose($temp_file);
?>
```
इस कोड को चलाने पर एक अस्थायी फ़ाइल बनती है, उसमें डेटा लिखा जाता है और जब काम खत्म हो जाता है, तो फ़ाइल अपने आप हट जाती है।

## Deep Dive (गहराई से जानकारी):

अस्थायी फ़ाइलें वर्चुअल 'तिजोरी' की तरह होती हैं, जिन्हें कम्प्यूटर की रैम या टेम्पररी डायरेक्ट्री में रखा जाता है। सिस्टम के रीस्टार्ट होने पर या `fclose()` फंक्शन के माध्यम से खत्म किए जाने पर ये डिलीट हो जाती हैं। `tmpfile()` फ़ंक्शन PHP में एक अस्थायी फ़ाइल क्रिएट करता है और एक हैंडल लौटाता है।

अस्थायी फ़ाइलें लिनक्स में `/tmp` या विंडोज़ में `C:\Windows\Temp` जैसी डायरेक्ट्री में बनाई जाती हैं। PHP के पुराने वर्जन में डेवलपर्स `temporary://` स्कीम का उपयोग करते थे, पर नए वर्जन में `tmpfile()` ज्यादा प्रचलित है।

अस्थायी फ़ाइल बनाने के अल्टरनेटिव में `tempnam()` और `sys_get_temp_dir()` फंक्शन्स होते हैं।

## See Also (अन्य संसाधन):

- PHP Manual on temp files: [php.net/manual/en/function.tmpfile.php](https://www.php.net/manual/en/function.tmpfile.php)
- PHP Manual on file system functions: [php.net/manual/en/ref.filesystem.php](https://www.php.net/manual/en/ref.filesystem.php)
- W3Schools PHP file handling: [w3schools.com/php/php_file.asp](https://www.w3schools.com/php/php_file.asp)
