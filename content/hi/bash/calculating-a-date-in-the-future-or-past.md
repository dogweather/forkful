---
title:                "Bash: भविष्य या भूत की तारीख की गणना"
simple_title:         "भविष्य या भूत की तारीख की गणना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप हिन्दी में बश प्रोग्रामिंग सीखने के बारे में सोच रहे हैं, तो आसानी से तारीख के भविष्य या भूत की गणना को समझना आपको बहुत आनंद देगा।

## कैसे करें

इस ब्लॉग पोस्ट के माध्यम से हम आपको तारीख की गणना करने के लिए बश प्रोग्रामिंग के सरल तरीके बताएंगे। यह काम आप अपने मूल बश स्क्रिप्ट में कर सकते हैं। इसके लिए हमारे पास एक आसान उदाहरण है:

```Bash
# आत्म संसोधन लें
read -p "तारीख को कितने दिन आगे या पीछे गणना करना है? " days

# पिछली या भविष्य की तारीख को निकालें
date -d "$days days" +"%d/%m/%Y"
```

अंतिम में, आपको सभी आवश्यक स्थानों पर सही कोड के साथ एक ही लाइन में दिए गए आरएमटीडीडी कमांड का उपयोग जरूर करना होगा।

## गहराई में जाएं

तारीख के बदलते स्वरूप के पीछे गहराई में जाने के लिए, आपको बश में अधिक जानकारी की आवश्यकता हो सकती है। यदि आपको थोड़ी सी भूमिका और शब्दावली में सवाल हो तो आप नीचे दी गई लिंक्स का उपयोग कर सकते हैं:

- [Bash गाइड](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [रैंडम तारीख के साथ काम करना](https://linuxcommando.blogspot.com/2008/05/how-to-generate-random-date-in-shell.html)
- [WikiHow में बश स्क्रिप्टिंग सीखें](https://www.wikihow.com/Learn-to-Write-Bash-Scripts)
- [लिनक्स के लिए बश स्क्रिप्टिंग](https://www.linuxjournal.com/article/10699)

## इससे जुड़ा और भी

- [Linux में बश स्क्रिप