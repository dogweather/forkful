---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- hi/bash/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:03.017942-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्य या अतीत में तिथि की गणना का मतलब है एक निश्चित तारीख से मनचाहे दिनों का जोड़ना या घटाना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे विशेष तिथियों का पता लगा सकें, जैसे कि प्रोजेक्ट्स की डेडलाइंस या इवेंट्स की तारीखें। 

## कैसे करें:
```Bash
# आज से 10 दिन बाद की तारीख
date -d "+10 days" '+%Y-%m-%d'

# आज से 3 सप्ताह पहले की तारीख
date -d "-3 weeks" '+%Y-%m-%d'

# एक निश्चित तारीख से 5 महीनें पहले की तारीख
date -d "2023-03-15 -5 months" '+%Y-%m-%d'
```
उदाहरण आउटपुट:
```Bash
# आज से 10 दिन बाद
2023-04-21

# आज से 3 सप्ताह पहले
2023-03-10

# 2023-03-15 से 5 महीनें पहले
2022-10-15
```

## गहराई से जानकारी:
तारीखों की गणना समय और कैलेंडर के प्रबंधन का मूलभूत हिस्सा है, जो सदियों से व्यापार, खेती, और आयोजनों में महत्वपूर्ण रहा है। Bash में `date` कमांड लिनक्स सिस्टम्स पर आधारित है और इसका उपयोग करके विभिन्न मापदंडों के साथ पिछली या आने वाली तारीखों की गणना की जा सकती है। विकल्प के तौर पर, अन्य शैलियों में Python में `datetime` मॉड्यूल या PHP में `strtotime` फंक्शन का उपयोग होता है। विवरणों में, बश का `date` कमांड GNU Coreutils पैकेज का हिस्सा है और यह Gregorian कैलेंडर के मानकों का पालन करता है।

## और भी देखें:
- GNU Coreutils Official Documentation: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/abs-guide.html
- Bash Date Command Tutorial: https://linuxize.com/post/linux-date-command/
