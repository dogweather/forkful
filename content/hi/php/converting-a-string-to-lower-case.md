---
title:    "PHP: स्ट्रिंग को लोअर केस में बदलना"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## क्यों

कई बार हमें अपने कोड में स्ट्रिंग्स को मैनिपुलेट करने की जरूरत पड़ती है, जैसे की उन्हें लोअर केस में बदलना। इससे हमारे कोड को दुहराने से बचाता है और स्ट्रिंग को समझने आसान बनाता है।

## हाउ टू

```PHP
$string = "HINDI IS AWESOME";
echo strtolower($string);
```
आउटपुट: hindi is awesome

## डीप डाइव

स्ट्रिंग को लोअर केस में बदलने के लिए PHP में `strtolower()` फ़ंक्शन का प्रयोग किया जाता है। यह फ़ंक्शन स्ट्रिंग को लोअर केस में बदलता है और नए स्ट्रिंग को वापस देता है। यदि हम उसी स्ट्रिंग को फिर से उपयोग में लाना चाहते हैं तो हमें उसे एक पहले डिफाइन किया गया वेरिएबल में स्टोर करना होगा।

## देखें भी

- [PHP strtoupper() function](https://www.php.net/manual/en/function.strtoupper.php)
- [PHP ucfirst() function](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP strrev() function](https://www.php.net/manual/en/function.strrev.php)