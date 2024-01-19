---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Command line arguments कुछ पैस के तरह होते हैं जो आपके PHP script को दिए जाते हैं। ये आपके प्रोग्राम को मोडिफाई करने में मदद करते हैं। आमतौर पर, विशेष विकल्पों, उपयोगकर्ता मापदंड, या फ़ाइल पथ के साथ-साथ इसे उपयोग किया जाता है।

## कैसे:

```PHP
// PHP script to read command line arguments
// Get arguments
$options = getopt(null, ["user:", "path:"]);

// Print arguments
foreach ($options as $key => $value) {
    echo "Key: $key; Value: $value\n";
}
```

यदि यह script को "--user=test --path=/tmp" परमिटर के साथ चलाया जाता है, तो यह output देगा:

```PHP
Key: user; Value: test
Key: path; Value: /tmp
```

## गहरी जांच:

PHP में command line arguments को पढ़ना बहुत साधारण होता है, फिर भी बिते समय में कुछ विभिन्न तरीकों का निर्माण हुआ है। उपयोगकर्ता मापदंडों में ``getopt()`` का उपयोग परमिटर्स को सीधे कैप्चर करने के लिए होता है।

विकल्प रूप में, आप ``$_SERVER['argv']`` में पाए जाने वाले command line arguments का उपयोग कर सकते हैं। यह अधिक flexible विकल्प हो सकता है, लेकिन यह परमिटर्स को manual parse करने की आवश्यकता हो सकती है।

```PHP
// Manually parsing command line parameters
$args = $_SERVER['argv'];
foreach ($args as $key => $value) {
    echo "Key: $key; Value: $value\n";
}
```

## देखें भी:

- [PHP Command line usage](https://www.php.net/manual/en/features.commandline.usage.php) - PHP Manual
- [Manual parsing of command line arguments](https://www.jonathan-petitcolas.com/2014/01/27/parsing-command-line-arguments-in-php.html) - Jonathan Petitcolas
- [PHP getopt](https://www.php.net/manual/en/function.getopt.php) - PHP Manual