---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:30:11.106393-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML पार्सिंग का मतलब है HTML डाटा को पढ़ने और उससे ज़रूरी जानकारी निकालना। प्रोग्रामर्स इस प्रक्रिया को वेब स्क्रैपिंग, HTML डाटा एनालिसिस या वेब इंटरैक्शंस के लिए करते हैं।

## How to: (कैसे करें:)
चलिए, देखते हैं कुछ सिंपल कोड के उदाहरण जो कि HTML को पार्स कर सकते हैं। याद रखें, बैश में डायरेक्ट HTML पार्सिंग इतनी आसान नहीं होती और इसके लिए अक्सर अतिरिक्त टूल्स की जरूरत होती है, जैसे `xmllint` जो कि `libxml` पैकेज का हिस्सा है।

```Bash
# HTML से टाइटल टैग को निकाले
html_content='<html><head><title>मेरा वेबपेज</title></head></html>'
title=$(echo $html_content | grep -oP '(?<=<title>).*(?=</title>)')
echo "वेबपेज का टाइटल है: $title"
```

आउटपुट:

```
वेबपेज का टाइटल है: मेरा वेबपेज
```

## Deep Dive (गहराई से जानकारी)
HTML पार्सिंग का इतिहास वेब डेवलपमेंट के शुरुआत से जुड़ा हुआ है। पारंपरिक रूप से, पार्सिंग के लिए काम्पलेक्स लाइब्रेरीज़ का इस्तेमाल होता था जैसे कि Perl's HTML::Parser या Python की Beautiful Soup। लेकिन Bash में, एक बार 'xmllint' या समान उपकरणों का आधार स्थापित हो जाने के बाद, हम उन्हें वेब डेटा के साथ काम करने के लिए उपयोग कर सकते हैं।

बैश में सीधे HTML पार्सिंग से बचना चाहिए क्योंकि यह टैग के मामूली बदलाव से टूट सकता है। रोबस्ट HTML पार्सिंग के लिए पूरी तरह से डेवलप किए गए टूल और लाइब्रेरीज़ की सिफारिश की जाती है।

## See Also (और भी जानकारी)
- [HTML पार्सिंग के लिए Beautiful Soup डॉक्युमेंटेशन](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [xmllint मैन्युअल](http://xmlsoft.org/xmllint.html)
- [Bash प्रोग्रामिंग इंट्रोडक्शन](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
