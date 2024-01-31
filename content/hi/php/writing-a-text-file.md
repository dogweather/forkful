---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

PHP में टेक्स्ट फाइल लिखना डेटा संग्रहण और पुन: प्राप्ति का एक सरल माध्यम है। प्रोग्रामर इसे कॉन्फिगरेशन, लॉग फ़ाइलों, या डेटा एक्सचेंज के लिए करते हैं।

## How to (कैसे करें):

PHP में एक टेक्स्ट फाइल बनाना और उसमें लिखना बहुत सीधा है। यहाँ आपको एक सरल उदाहरण मिलेगा:

```PHP
<?php
$filename = "example.txt";
$content = "यह पहली पंक्ति है।\nऔर यह दूसरी।";

// फाइल खोलें, अगर नहीं है तो बनाएं
$file = fopen($filename, "w");

// जाँचें की फाइल खुली की नहीं
if ($file) {
    fwrite($file, $content);    // सामग्री लिखें
    fclose($file);              // फाइल बंद करें
} else {
    echo "फाइल नहीं खुल सकी!";
}
?>
```

उपरोक्त कोड `example.txt` फाइल में टेक्स्ट लिखता है।

## Deep Dive (गहराई से जानकारी)

PHP में फाइलें लिखने का इतिहास काफी पुराना है और `fopen()` व `fwrite()` जैसे फंक्शन्स इसके मूल हैं। विकल्पों में `file_put_contents()` शामिल है, जो केवल एक पंक्ति में फाइल लिखने की प्रक्रिया को समेटता है। फिर भी, सुरक्षा और लॉकिंग जैसे विस्तृत तत्वों को संभालने के लिए, अधिक नियंत्रण के लिए `fopen()` का उपयोग अधिक है।

## See Also (और भी देखें)

- PHP Official Documentation for file handling: [PHP: Filesystem Functions](https://www.php.net/manual/en/ref.filesystem.php)
- W3Schools PHP File handling: [PHP 5 File Create/Write](https://www.w3schools.com/php/php_file_create.asp)
