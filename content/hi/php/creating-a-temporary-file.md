---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "PHP: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
एक वैपसू शैली और सफलता के लिए बहुत आवश्यकता होती है - अस्थायी फाइल बनाना। यह उन प्रोग्रामरों के लिए महत्वपूर्ण है जो अपने कोड की वैलिडेशन के दौरान अस्थायी फ़ाइलों का उपयोग करते हैं।

## कैसे: 
अस्थायी फ़ाइल बनाने के लिए, आपको `tempnam()` या `tmpfile()` फ़ंक्शन का उपयोग करना होगा। इन्हें आपको स्थायी फ़ाइल के साथ उसी फ़ोल्डर में फ़ाइल नाम देने के लिए प्रोग्राम सुबिधा के रूप में भी जोड़ना होगा।
```
// Example using tempnam()
$temp_file = tempnam("/tmp", "article"); // Creates a temporary file in the /tmp directory with the prefix "article"
echo $temp_file; // Output: /tmp/articleSxAN58

// Example using tmpfile()
$temp_file = tmpfile();
echo $temp_file; // Output: Resource id #1
```
यहां, हमने दो विभिन्न फ़ंक्शन उपयोग किए हैं, जो अस्थायी फ़ाइल बनाने और कोड में उसके साथ काम करने की सुविधा प्रदान करते हैं।

## गहराई में: 
अस्थायी फ़ाइल बनाने के पीछे एक इतिहास है। इसका उपयोग प्राचीन प्रोग्रामिंग में व्यापकता के लिए किया जाता था, जहां कार्यों को करने के लिए कम जगह थीं। लेकिन आज भी, यह प्रोग्रामरों के लिए उपयोगी है जो आपसे बाइंडिंग फ़ाइल आदि से बचना चाहते हैं। विकल्प तरीके भी मौजूद हैं, जैसे कि स्थायी फ़ाइल बनाना या सिस्टम भंडारण में उन्हें बैकअप करना।

## जुड़े कुछ भी: 
अधिक जानकारी के लिए, PHP के [आधिकारिक शैली गाइड](https://www.php.net/manual/en/function.tempnam.php) का उपयोग कर सकते हैं। यहां आपको `tempnam()` और `tmpfile()` फ़ंक्शन के अलावा अन्य अस्थायी फ़ाइल बनाने के तरीके भी मिलेंगे।