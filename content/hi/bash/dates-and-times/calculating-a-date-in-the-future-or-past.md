---
date: 2024-01-20 17:31:03.017942-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.649633-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

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
