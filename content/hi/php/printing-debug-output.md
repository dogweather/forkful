---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## व्हाट एंड व्हाई?

`समस्या निवारण` का उद्देश्य हमारे कोड की गठन को समझना और कोड में होने वाली त्रुटियों को पहचानना है। वैविध्यमय विस्तार और जटिलता के कारण, प्रोग्रामर्स इसे अपने कोड के व्यवहार को विश्लेषण करने और संसधनों का पता लगाने के लिए करते हैं। 

## हाउ तु:

```PHP 
<?php
$error = 'This is an error message.';
echo 'Debug: ' . $error . "\n";
?>
```
ऊपरी কोड निम्नलिखित होगा:

```PHP
Debug: This is an error message.
```
## डीप डाइव:

आइटी की दुनिया में `समस्या निवारण` का इतिहास जितना ही पुराना हैं जितना की प्रोग्रामिंग का। पहले लोग बिना किसी विशेष उपकरण का उपयोग किए इसे करते थे जो बहुत ही कठिन था। `echo` और `print` फंक्शन PHP में डिबग के लिए सबसे आम रूप से उपयोग किए जाते हैं, हालांकि `var_dump` और `print_r` फ़ंक्शन भी उपयोग किए जाते हैं जब आपको एक विस्तृत दृश्य की जरूरत होती है।

## देखें भी:

1. [PHP Manual: Debugging in PHP](https://www.php.net/manual/en/debugger.php)