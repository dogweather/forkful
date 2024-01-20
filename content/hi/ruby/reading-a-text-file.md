---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक पाठ फ़ाइल को पढ़ना मतलब, उसमें किसी भी प्रकार की डेटा की खोज करना जिसे हम एक कंप्यूटर प्रोग्राम में उपयोग कर सकते हैं। कार्यक्रमकर्ता इसे क्यों करते हैं? डेटा को उत्‍पादन में लाने के लिए - टेस्ट करने के लिए, रिपोर्ट्स बनाने के लिए या सिर्फ़ देखने के लिए!

## कैसे करें:
Ruby में, हम `File` कक्ष का उपयोग कर सकते हैं किसी भी फ़ाइल को पढ़ने के लिए।

```Ruby
file = File.open("sample.txt", "r")
while (line = file.gets)
    puts line
end
file.close
```
इससे "sample.txt" नामक फ़ाइल की सभी पंक्तियों को स्क्रीन पर प्रिंट किया जाएगा।

## गहरी माहिती:
पाठ फ़ाइल को पढ़ने के लिए, Ruby प्रोग्रामिंग भाषा का इतिहास लंबा है। जब Ruby फ़ाइलों के साथ काम करने के लिए विकसित हुई थी, तो यह`File` कक्ष का परिचय दी थी। आज भी, `File` कक्ष का उपयोग यही बना हुआ है, हालांकि पाठ फ़ाइल को पढ़ने के अन्य तरीके - जैसे कि `IO` कक्ष - अब उपलब्ध हैं। इन तरीकों में से कौन सा सबसे अच्छा है, यह आपके काम पर निर्भर करता है।

## भी देखें:
1. [Ruby का अधिकृत दस्तावेज़ीकरण](https://ruby-doc.org/core-2.7.0/File.html)
2. [GeeksforGeeks पर Ruby File IO](https://www.geeksforgeeks.org/ruby-file-io/)

Note: This article is targeted towards Hindi readers who already have a basic understanding of computer programming concepts and Ruby programming.