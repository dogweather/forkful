---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:33.937800-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
रैंडम नंबर्स वह होते हैं जो किसी निश्चित पैटर्न या ऑर्डर का पालन नहीं करते। प्रोग्रामर्स रैंडम नंबर तब पैदा करते हैं जब उन्हें टेस्टिंग, गेमिंग, सिक्योरिटी, या सिमुलेशन में अनप्रेडिक्टेबल रिजल्ट्स की जरूरत होती है।

## कैसे करें? (How to:)
```Ruby
# रैंडम नंबर्स जनरेट करने के लिए Ruby का प्रयोग
random_number = rand(100) # 0 से 99 के बीच एक अंक
puts random_number

# रैंडम नंबर्स एक रेंज में
random_range_number = rand(10..20) # 10 से 20 के बीच एक अंक
puts random_range_number

# सीडेड रैंडम नंबर्स (प्रेडिक्टेबल पैटर्न वाले)
srand(1234)
seeded_random = rand(100)
puts seeded_random
```
संभावित आउटपुट:
```
23 (यह नंबर हर बार बदलेगा)
15 (यह नंबर हर बार बदलेगा)
47 (यह नंबर हर बार वही रहेगा जब सीड समान हो)
```

## गहराई में (Deep Dive)
रैंडम नंबर्स का इतिहास कंप्यूटर साइंस में बहुत पुराना है। सबसे पहले, हार्डवेयर आधारित रेंडमनेस (उदा. रेडियो एक्टिव डिके) का इस्तेमाल होता था, लेकिन आजकल प्यूडो-रैंडम नंबर जेनरेटर्स (PRNGs) अधिक प्रचलित हैं। Ruby का `rand` एक PRNG है। `srand` फंक्शन से सीड सेट कर देने पर हमेशा एक समान पैटर्न वाले रैंडम नंबर्स आते हैं - इसका फायदा टेस्टिंग में होता है। इसका इंटर्नल इम्प्लीमेंटेशन Mersenne Twister एल्गोरिथ्म पर आधारित है जो एक high-quality PRNG माना जाता है।

## और भी जानें (See Also)
- Ruby के `Random` क्लास का डॉक्यूमेंटेशन [रूपी डॉक्स](https://ruby-doc.org/core/Random.html)
- रैंडम नंबर्स और सिक्योरिटी पर गाइड [OWASP रैंडमनेस गाइड](https://owasp.org/www-community/vulnerabilities/Insufficient_Entropy)
- Mersenne Twister एल्गोरिदम की जानकारी [Mersenne Twister](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)