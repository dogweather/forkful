---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:52.433176-07:00
description: "\u0915\u0948\u0938\u0947: \u0930\u0942\u092C\u0940 \u092B\u093E\u0907\
  \u0932 \u0911\u092A\u0930\u0947\u0936\u0928\u094D\u0938 \u0915\u094B \u0938\u0930\
  \u0932 \u092C\u0928\u093E\u0924\u0940 \u0939\u0948\u0964 \u090F\u0915 \u092B\u093E\
  \u0907\u0932 \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F, \u0906\u092A \u0930\u0942\u092C\u0940 \u0915\u0947 \u092C\u093F\u0932\
  \u094D\u091F-\u0907\u0928 `File` \u0915\u094D\u0932\u093E\u0938 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u0928\u093F\u092E\u094D\u0928\u0932\u093F\u0916\u093F\u0924 \u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0926\u093F\u0916\u093E\u0924\u093E \u0939\u0948 \u0915\
  \u093F\u2026"
lastmod: '2024-03-13T22:44:53.256469-06:00'
model: gpt-4-0125-preview
summary: "\u0930\u0942\u092C\u0940 \u092B\u093E\u0907\u0932 \u0911\u092A\u0930\u0947\
  \u0936\u0928\u094D\u0938 \u0915\u094B \u0938\u0930\u0932 \u092C\u0928\u093E\u0924\
  \u0940 \u0939\u0948\u0964 \u090F\u0915 \u092B\u093E\u0907\u0932 \u092E\u0947\u0902\
  \ \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A \u0930\
  \u0942\u092C\u0940 \u0915\u0947 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `File`\
  \ \u0915\u094D\u0932\u093E\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0928\u093F\u092E\u094D\
  \u0928\u0932\u093F\u0916\u093F\u0924 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\
  \u093F\u0916\u093E\u0924\u093E \u0939\u0948 \u0915\u093F \u0915\u0948\u0938\u0947\
  \ \u0932\u0947\u0916\u0928 (`\"w\"` \u092E\u094B\u0921) \u0914\u0930 \u091C\u094B\
  \u0921\u093C\u0928\u0947 (`\"a\"` \u092E\u094B\u0921) \u0915\u0947 \u0932\u093F\u090F\
  \ \u090F\u0915 \u092B\u093E\u0907\u0932 \u0915\u094B \u0916\u094B\u0932\u0928\u093E\
  , \u0909\u0938\u092E\u0947\u0902 \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u0932\u093F\u0916\u0928\u093E, \u0914\u0930 \u0909\u0938\u0915\u0947\
  \ \u092C\u093E\u0926 \u092B\u093E\u0907\u0932 \u0915\u094B \u092C\u0902\u0926 \u0915\
  \u0930\u0928\u093E \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\
  \u0930\u0928\u093E."
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u0932\u093F\u0916\u0928\u093E"
weight: 24
---

## कैसे:
रूबी फाइल ऑपरेशन्स को सरल बनाती है। एक फाइल में लिखने के लिए, आप रूबी के बिल्ट-इन `File` क्लास का उपयोग कर सकते हैं। निम्नलिखित उदाहरण दिखाता है कि कैसे लेखन (`"w"` मोड) और जोड़ने (`"a"` मोड) के लिए एक फाइल को खोलना, उसमें एक स्ट्रिंग लिखना, और उसके बाद फाइल को बंद करना सुनिश्चित करना:

```ruby
# एक फाइल में नई सामग्री लिखना, मौजूदा सामग्री को ओवरराइट करना
File.open("example.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end

# एक फाइल के अंत में सामग्री जोड़ना
File.open("example.txt", "a") do |file|
  file.puts "Adding another line."
end
```
दोनों स्निपेट्स को चलाने के बाद, `example.txt` की सामग्री होगी:
```
Hello, Ruby!
Adding another line.
```

### थर्ड-पार्टी लाइब्रेरी का उपयोग: FileUtils
अधिक जटिल फाइल ऑपरेशन्स के लिए, रूबी स्टैंडर्ड लाइब्रेरी `FileUtils` काम आ सकती है, हालांकि मूल फाइल लेखन के लिए स्टैंडर्ड `File` तरीके पर्याप्त हैं। फिर भी, यदि आप फाइल लेखन के संयोजन में कॉपी, मूव, रिमूव या अन्य फाइल सिस्टम ऑपरेशन्स करना चाहते हैं, तो `FileUtils` का पता लगाना उपयोगी है।

उस निर्देशिका में एक फाइल में लिखने के लिए `FileUtils` का उपयोग करने का एक उदाहरण:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Log entry: #{Time.now}"
end
```

यह दिखाता है कि यदि वह पहले से मौजूद नहीं है, तो नई निर्देशिका `logs` कैसे बनाई जाती है, और उसमें एक नई फाइल `today.log` में लिखना, बिना सीधे FileUtils से लिखे हुए लेकिन इसकी निर्देशिका हैंडलिंग क्षमता का उपयोग करके निर्देशिका और फाइल मैनिपुलेशन दोनों को प्रदर्शित करना।
