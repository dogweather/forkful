---
date: 2024-01-26 00:57:51.652635-07:00
description: "\u090F\u0930\u0930 \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917\
  \ \u0915\u093E \u0924\u093E\u0924\u094D\u092A\u0930\u094D\u092F \u0915\u094B\u0921\
  \ \u092E\u0947\u0902 \u0905\u092A\u094D\u0930\u0924\u094D\u092F\u093E\u0936\u093F\
  \u0924 \u0915\u0940 \u0906\u0936\u093E \u0915\u0930\u0928\u093E \u0939\u0948 \u2014\
  \ \u092C\u093F\u0928\u093E \u0915\u094D\u0930\u0948\u0936 \u0915\u093F\u090F \u0939\
  \u0941\u090F \u0917\u0932\u0924\u093F\u092F\u094B\u0902 \u0914\u0930 \u0938\u092E\
  \u0938\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0938\u0941\u091A\u093E\u0930\
  \u0941 \u0930\u0942\u092A \u0938\u0947 \u092A\u094D\u0930\u092C\u0902\u0927\u093F\
  \u0924 \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:53.237049-06:00'
model: gpt-4-1106-preview
summary: "\u090F\u0930\u0930 \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917 \u0915\
  \u093E \u0924\u093E\u0924\u094D\u092A\u0930\u094D\u092F \u0915\u094B\u0921 \u092E\
  \u0947\u0902 \u0905\u092A\u094D\u0930\u0924\u094D\u092F\u093E\u0936\u093F\u0924\
  \ \u0915\u0940 \u0906\u0936\u093E \u0915\u0930\u0928\u093E \u0939\u0948 \u2014 \u092C\
  \u093F\u0928\u093E \u0915\u094D\u0930\u0948\u0936 \u0915\u093F\u090F \u0939\u0941\
  \u090F \u0917\u0932\u0924\u093F\u092F\u094B\u0902 \u0914\u0930 \u0938\u092E\u0938\
  \u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0938\u0941\u091A\u093E\u0930\u0941\
  \ \u0930\u0942\u092A \u0938\u0947 \u092A\u094D\u0930\u092C\u0902\u0927\u093F\u0924\
  \ \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F\u2026"
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

एरर हैंडलिंग का तात्पर्य कोड में अप्रत्याशित की आशा करना है — बिना क्रैश किए हुए गलतियों और समस्याओं को सुचारु रूप से प्रबंधित करना। प्रोग्रामर इसे इसलिए करते हैं ताकि जब चीजें गलत हों तो फ्लो को नियंत्रित कर सकें और उपयोगकर्ता अनुभव को स्मूथ रख सकें।

## कैसे करें:

Ruby में `begin`, `rescue`, `ensure`, और `end` का उपयोग करके एरर्स को हैंडल किया जाता है। आप जोखिम भरे कोड को `begin` और `end` के बीच में लपेटते हैं। यदि कोई एरर होता है, तब `rescue` काम करता है।

```Ruby
begin
  # जोखिम भरा कोड यहाँ जाएगा।
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "उफ़! आप ऐसा नहीं कर सकते: #{e.message}"
ensure
  puts "यह हमेशा चलेगा, चाहे एरर हो या ना हो।"
end
```

नमूना आउटपुट:
```
उफ़! आप ऐसा नहीं कर सकते: divided by 0
यह हमेशा चलेगा, चाहे एरर हो या ना हो।
```

## गहन अध्ययन

ऐतिहासिक रूप से, प्रोग्रामिंग भाषाओं में एरर हैंडलिंग काफी विकसित हुई है, जहाँ पहले की भाषाओं में अक्सर साधारण या गैर-मौजूद तंत्र होते थे। Ruby की एक्‍सेप्शन हैंडलिंग Python और Smalltalk जैसी भाषाओं से प्रेरित है।

Ruby में `begin-rescue` के विकल्पों में विधि परिभाषाओं में `rescue` का उपयोग या गैर-मानक फ्लो कंट्रोल के लिए `throw` और `catch` का उपयोग करना शामिल है, हालांकि उनका उपयोग सामान्य एरर हैंडलिंग के लिए नहीं किया जाता है।

एक दिलचस्प विवरण: Ruby में अपवाद (एक्सेप्शन) ऑब्जेक्ट होते हैं (Exception क्लास और उसके वंशजों के उदाहरण), इसलिए आप कस्टम एरर क्लासेस को परिभाषित कर सकते हैं और सिर्फ एरर्स को लॉग करने से ज्यादा कर सकते हैं - आप प्रोग्राम के चारों ओर समृद्ध स्थिति ले जा सकते हैं, जो और अधिक मजबूत एरर हैंडलिंग के लिए है।

## देखें भी

- अपवादों और एरर हैंडलिंग पर Ruby दस्तावेज़: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Ruby एरर हैंडलिंग की बेहतरीन प्रैक्टिसेज पर विस्तारित गाइड: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
