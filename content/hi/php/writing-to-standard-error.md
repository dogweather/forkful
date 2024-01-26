---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्टैण्डर्ड एरर में लिखना का मतलब गलतियों की जानकारी एक खास आउटपुट में भेजना है। प्रोग्रामर यह इसलिए करते हैं ताकि एरर्स को सामान्य आउटपुट से अलग किया जा सके और डीबगिंग में आसानी हो।

## कैसे करें:
```php
<?php
// सामान्य आउटपुट
echo "यह मैसेज स्टैण्डर्ड आउटपुट पर जाएगा।\n";

// स्टैण्डर्ड एरर पर लिखना
fwrite(STDERR, "यह मैसेज स्टैण्डर्ड एरर पर जाएगा।\n");
?>
```
सैंपल आउटपुट:
```
यह मैसेज स्टैण्डर्ड आउटपुट पर जाएगा।
यह मैसेज स्टैण्डर्ड एरर पर जाएगा।
```

## गहराई में:
स्टैण्डर्ड एरर (stderr) एक परंपरागत स्ट्रीम है जो कि शुरुआत से यूनिक्स लाइक ऑपरेटिंग सिस्टम्स में है। `fwrite()` फ़ंक्शन का इस्तेमाल करके हम स्टैण्डर्ड एरर में लिख सकते हैं। इसके अलावा, `STDERR` कॉन्स्टेंट में पहले से ही एरर आउटपुट हैंडल को PHP में डिफाइन किया गया है। ऐल्टरनेटिव के रूप में लॉग फाइल्स का उपयोग किया जा सकता है।

## और भी देखें:
- PHP मैनुअल पर [fwrite](https://www.php.net/manual/en/function.fwrite)
- [Standard streams](https://en.wikipedia.org/wiki/Standard_streams) पर विकिपीडिया लेख
- [Error Handling](https://www.php.net/manual/en/book.errorfunc.php) पर PHP मैनुअल सेक्शन
