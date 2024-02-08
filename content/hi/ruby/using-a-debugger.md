---
title:                "डीबगर का उपयोग करना"
aliases:
- hi/ruby/using-a-debugger.md
date:                  2024-01-26T04:10:48.130720-07:00
model:                 gpt-4-0125-preview
simple_title:         "डीबगर का उपयोग करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रूबी में एक डिबगर का उपयोग करके प्रोग्रामर्स को अपने कोड को रोकने, वेरिएबल्स की जाँच करने और अपने कोड को लाइन-दर-लाइन चलाने की एक सुपरपावर मिलती है। लोग इसे कीड़े को खत्म करने, कोड प्रवाह को समझने, और यह देखने के लिए करते हैं कि उनके लिखे गए जादू (कोड) तब क्या कर रहे हैं जब जादू होता है—या नहीं होता।

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
