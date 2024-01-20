---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग से उद्देश्य होता है वेब पेज कंटेंट को पढ़ना और समझना। इसका मुख्यातः उपयोग वेब स्क्रेपिंग से डाटा निकालने में है।

## कैसे करें:

```Fish Shell 
set url https://www.example.com
curl -sL $url | pup 'p text{}'
```

ऊपर दिए गए कोड में *,curl* HTML पेज को डाउनलोड करने के लिए, *pup* HTML पार्सिंग यन्त्र है। 

## गहराई में: 

HTML पार्सिंग का इतिहास CGI (Common Gateway Interface) से शुरू हुआ था। PHP, Python के BeautifulSoup और Ruby के Nokogiri library इत्यादि वैकल्पिक उपकरण हैं। Fish Shell में 'pup' उपकरण, Go भाषा पर आधारित है, जो बहुत ही तेज और विश्वसनीय है।