---
date: 2024-01-26 04:10:48.130720-07:00
description: "\u0930\u0942\u092C\u0940 `byebug` \u0928\u093E\u092E\u0915 \u090F\u0915\
  \ \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u0921\u093F\u092C\u0917\u0930 \u0915\
  \u0947 \u0938\u093E\u0925 \u0906\u0924\u0940 \u0939\u0948\u0964 \u092A\u0939\u0932\
  \u0947, \u0905\u092A\u0928\u0940 Gemfile \u092E\u0947\u0902 `byebug` \u0915\u094B\
  \ \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0947\u0902 \u0914\u0930 `bundle install`\
  \ \u091A\u0932\u093E\u090F\u0901\u0964 \u092B\u093F\u0930, \u0905\u092A\u0928\u0947\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u091C\u0939\
  \u093E\u0901 \u0906\u092A\u2026"
lastmod: '2024-03-13T22:44:53.231868-06:00'
model: gpt-4-0125-preview
summary: "\u0930\u0942\u092C\u0940 `byebug` \u0928\u093E\u092E\u0915 \u090F\u0915\
  \ \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u0921\u093F\u092C\u0917\u0930 \u0915\
  \u0947 \u0938\u093E\u0925 \u0906\u0924\u0940 \u0939\u0948\u0964 \u092A\u0939\u0932\
  \u0947, \u0905\u092A\u0928\u0940 Gemfile \u092E\u0947\u0902 `byebug` \u0915\u094B\
  \ \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0947\u0902 \u0914\u0930 `bundle install`\
  \ \u091A\u0932\u093E\u090F\u0901\u0964 \u092B\u093F\u0930, \u0905\u092A\u0928\u0947\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u091C\u0939\
  \u093E\u0901 \u0906\u092A\u2026"
title: "\u0921\u0940\u092C\u0917\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E"
weight: 35
---

## कैसे करें:
रूबी `byebug` नामक एक निर्मित डिबगर के साथ आती है। पहले, अपनी Gemfile में `byebug` को शामिल करें और `bundle install` चलाएँ। फिर, अपने प्रोग्राम को जहाँ आप विश्राम चाहते हैं, wहाँ `byebug` डालें।

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

इस स्क्रिप्ट को चलाने पर `byebug` पर निष्पादन रुक जाएगा, और आपको एक इंटरैक्टिव सत्र में फेंक दिया जाएगा जहाँ आप आदेशों जैसे टाइप कर सकते हैं:

```
step
next
continue
var local
```

नमूने का आउटपुट आपको ऐसा प्रॉम्प्ट दिखाएगा:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:  byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## गहराई में:
`byebug` से पहले, रूबीवासी `debugger` और `pry` का उपयोग करते थे। बाद वाले, `pry`, केवल एक डिबगर से अधिक है; यह एक शक्तिशाली REPL है जिसे `binding.pry` ब्रेकपॉइंट के साथ डिबगिंग के लिए भी उपयोग किया जा सकता है।

रूबी के `byebug` के विकल्प में `pry-byebug` शामिल है, जो `pry` के साथ `byebug` की कार्यक्षमता को जोड़ता है, और `ruby-debug`, जो एक पुराना गेम है जिसे अब सक्रिय रूप से विकसित नहीं किया जाता है।

जब आप `byebug` को बुलाते हैं, तो डिबगर आपके कोड निष्पादन को थाम लेता है और आपको रनटाइम में एक झलक देता है। आप वेरिएबल्स को देख और बदल सकते हैं, कोड में विभिन्न बिंदुओं पर कूद सकते हैं, और यहाँ तक कि कुछ रूबी कोड को लाइन-दर-लाइन चला भी सकते हैं। यह किंडा है जैसे आपके रूबी कोड के लिए समय-यात्रा की क्षमताएँ हों।

## यह भी देखें:
- Byebug GitHub रिपॉजिटरी: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry दस्तावेज़ीकरण: [https://github.com/pry/pry](https://github.com/pry/pry)
- Rails ऐप्स डिबगिंग के लिए एक गाइड: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
