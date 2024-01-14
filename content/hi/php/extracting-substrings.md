---
title:                "PHP: अलग-अलग स्ट्रिंग्स निकालना।"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप बेहतर PHP कोडिंग सीखना चाहते हैं तो उपस्थिति तक पहुँचने के लिए हमें सूचनाओं से भरपूर होने के लिए एक कदम आगे जाना होगा? स्ट्रिंग व्युिचारा को निकालना।

## कैसे
```PHP
<?php
$string = "यह हमारा स्ट्रिंग संग्रह है";
$substring = substr($string, 4, 7);
echo $substring;
?>
```
```
हमारा स्ट्रिंग
```

## गहराई में उतरना
स्ट्रिंग से सबसे संटूलता में जाने के लिए सबसे स्पष्ट अनुमान लगाने के लिए कोड कैसेज नवाचकके. पुनः ऐसी लाइक सुनसन नहीं। [क्या सभी समस्याओं और समाधानों का सबसे अहनुमान स्ट्रिंग व्युिचार का जरूर लो
https://www.php.net/manual/en/function.substr.php
https://www.w3schools.com/php/func_string_substr.asp

## इसके अलावा देखें
[PHP वीडियो शिक्षण (Hindi)](https://www.youtube.com/watch?v=LQxLudgGxg8)
[PHP के लिए सीखने के १० नियमों को लें](https://code.tutsplus.com/series/10-essential-things-to-learn-about-php--net-6144)