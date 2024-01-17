---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Fish Shell: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामर्स एक तारीख को स्ट्रिंग में बदलने के कारण अपने कोड में इस्तेमाल करते हैं। इस स्ट्रिंग विस्तार को पढ़ने और लिखने में आसानी होती है।

## कैसे:

फिश शैल में एक तारीख को स्ट्रिंग में बदलने के लिए आप `strftime` फंक्शन का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में एक उदाहरण है:

```fish
set date (strftime "%d %B %Y")
echo $date # 31 जनवरी 2020
```

## गहराई में जाएं:

तारीख को स्ट्रिंग में बदलने का प्रयोग बहुत पुराना है और प्रोग्रामिंग के फिल्ड में बहुत उपयोगी है। अगर आपको यह फंक्शन अच्छी तरह से नहीं आता है, तो आप `date` कमांड का भी उपयोग कर सकते हैं। यह भी आपको तारीख को स्ट्रिंग में बदलने की सुविधा प्रदान करता है।

## इस से जुड़े और भी दस्तावेज़:

- [Fish शैल मैनुअल](https://fishshell.com/docs/current/index.html): फिश शैल के बारे में अधिक जानकारी के लिए।
- [strftime मैनुअल](https://man7.org/linux/man-pages/man3/strftime.3.html): `strftime` फंक्शन के लिए और भी दस्तावेज़ का संदर्भ।