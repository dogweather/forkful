---
title:                "Ruby: स्ट्रिंग को कैपिटलाइज करना।"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना।"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

एक रुबी डेवलपर के लिए स्ट्रिंग को कैपिटलाइज करना एक आम टास्क है। यह फंक्शन स्ट्रिंग को उसके शुरूआती अक्षर को बड़ा होने के लिए प्रेरित करता है जो कि डेटा मोडलिंग और डिस्प्ले में उपयोगी हो सकता है। 

## कैसे करें

```Ruby
name = "rohan"
puts name.capitalize 
```
आउटपुट: Rohan

```Ruby
sentence = "mera pyaara desh, Bharat"
puts sentence.capitalize 
```
आउटपुट: Mera pyaara desh, bharat

## गहराई में ऊंचाई पाने के लिए

स्ट्रिंग कैपिटलाइज करने के लिए रुबी में बहुत से मार्थोड्स हैं जैसे split, collect और map आदि। आप स्ट्रिंग के प्रत्येक अक्षर को भी कैपिटल कर सकते हैं या अपनी विशेष मायने रखते हुए स्ट्रिंग को कैपिटलाइज कर सकते हैं। इसके अलावा आप Unicode और वर्गीकरण के साथ भी उन्नत कैपिटलाइजेशन कर सकते हैं। 

## देखें भी

- [How to Capitalize the First Letter of a String in Ruby](https://www.rubyguides.com/2018/03/ruby-capitalization/)
- [Ruby String Methods](https://www.rubyguides.com/2019/04/ruby-string-methods/)