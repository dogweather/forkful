---
title:    "PHP: टेक्स्ट खोजें और बदलें"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों?

क्या आपने कभी बहुत सारे टेक्स्ट को बदलने के लिए तरह-तरह की जगहों पर गंजागर्मी से ढूंड रखी है? संपादन चरों में बदलाव करने से पहले दुनिया का सबसे बोरिंग काम है। लेकिन PHP में सही तरीके से स्ट्रिंग को खोजें और बदलें, तो यह खेल का नाम हो जाता है।

## कैसे करें?

```PHP
$text = "यह एक शानदार दिन है।"

// शानदार को खोजें और उसे अद्यतन करें
$new_text = str_replace("शानदार", "भविष्यवाणी करते हुए", $text);

echo $new_text; // यह एक भविष्यवाणी करते हुए दिन है।
```

PHP में `str_replace()` फ़ंक्शन का उपयोग विशेष रूप से टेक्स्ट को खोजने और उसे बदलने के लिए किया जाता है। टेक्स्ट के स्थानांतरण भी किया जा सकता है। इसके अलावा, `str_replace()` कई टेक्स्ट स्ट्रिंग्स को खोजने और बदलने में उपयोगी हो सकता है।

## गहराई की खोज

जब PHP में शब्दों या वाक्यों को खोजने और उसे बदलने की बात आती है, तो `str_replace()` ही सबसे आसान और सबसे ज्यादा उपयोगी हो सकता है। इसके साथ-साथ, आप `str_replace()` का इस्तेमाल HTML कोड या सबसे अधिक उदाहरण में CSV फ़ाइल में भी कर सकते हैं।

## देखें भी

- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php)
- [Regular Expressions in PHP](https://www.php.net/manual/en/book.pcre.php)
- [PHP String Manipulation](https://www.w3schools.com/php/php_string.asp)