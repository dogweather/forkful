---
title:                "हटमल मैंथन (Hataumal mainthan)"
html_title:           "Fish Shell: हटमल मैंथन (Hataumal mainthan)"
simple_title:         "हटमल मैंथन (Hataumal mainthan)"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

HTML पार्सिंग में शामिल होने के *क्यों* कोई इच्छुक होगा।

क्योंकि यह वेरिएबल्स और HTML दस्तावेजों से डेटा को प्रकाशित करने के लिए एक शक्तिशाली तरीका है और यह सुरक्षित भी है। HTML पार्सिंग आसान है और आपको परिणामों को पटने की आवश्यकता नहीं होती है, इसलिए यह वेब साइटों बनाने, डेटा सर्वरों को एक्सेस करने अथवा विशेष ऑनलाइन टूल्स बनाने के लिए उपयोगी है ।

## कैसे करें

शुरू करने के लिए, हम [`curl`](https://fishshell.com/docs/current/cmds/curl.html) को इस्तेमाल करके एक वेब पन्ने का सामग्री डाउनलोड करते हैं:

```
curl -s http://example.com/page.html > page.html
```

अब हम [`cat`](https://fishshell.com/docs/current/cmds/cat.html) को इस्तेमाल करके उस फ़ाइल की सामग्री को पढ़ सकते हैं और उसका स्थानीय फ़ाइल प्रणाली में वेरिएबल्स में संगठित कर सकते हैं:

```
set -l title (curl -s -L http://example.com | sed 's/.*<title>\(.*\)<\/title.*/$1/')
```

इस उदाहरण में, हमने `curl` का प्रयोग एक और [`sed`](https://fishshell.com/docs/current/cmds/sed.html) के साथ किया है जो टैग को मैच और श्रृंखला स्थानान्तरित करता है। आप अपने HTML कोड के अन्य भागों को पटने के लिए उसी प्रक्रिया का अनुसरण कर सकते हैं।

## डीप डाइव

पार्स प्रक्रिया में आप पंक्तियाँ, प्रकार, classname और ids को पहचानने के लिए एक DOM पेशकश प्रयोग कर सकते हैं। Zend Framework भ्षजी को भ्षजी पोषित करने के लिए सभी "रास या पासिंग पटने के लिए DOM पूर्ण पत्तन पर YML का प