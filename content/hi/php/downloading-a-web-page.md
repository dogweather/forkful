---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पेज डाउनलोड करना मतलब है कि आप एक वेबसाइट से डाटा प्राप्त करते हैं और उसे अपने स्थानीय सिस्टम में स्टोर करते हैं। प्रोग्रामर्स इसका उपयोग API कॉल्स करने, वेबसाइट्स का डाटा एकत्र करने, या वेब पेजों की निगरानी करने के लिए करते हैं।

## कैसे:
यहां PHP का एक साधारण कोड है जो एक वेब पेज डाउनलोड करता है:
```PHP
<?php
$websiteContent = file_get_contents('http://example.com');

echo $websiteContent;
?>
```
आपको 'http://example.com' की जगह आपके टारगेट वेबसाइट का URL डालना होगा।

## डीप डाइव
वेब पेज डाउनलोड करने की क्षमता के पुन: स्थापना का इतिहास PHP 4 में शुरू होता है, जहां `file_get_contents()` फंक्शन पहली बार पेश किया गया था। आंतरिक रूप से यह बीमा सोकेट आधारित आवेदन प्रोटोकॉल पर काम करता है। इसका विकल्प `cURL` हो सकता है, जो अधिक विस्तृत है और अधिक सेटिंग्स का समर्थन करता है। 

## देखिए भी
अधिक अध्ययन के लिए, ये उपयोगी संसाधन हो सकते हैं:
1. PHP Manual's page on file_get_contents(): https://www.php.net/manual/en/function.file-get-contents.php
2. PHP Manual's cURL section: https://www.php.net/manual/en/book.curl.php
3. Stackoverflow discussion on file_get_contents() vs cURL: https://stackoverflow.com/questions/555523/file-get-contents-vs-curl-what-has-better-performance