---
title:                "स्ट्रिंग को निचले अक्षर में रूपांतरित करना"
html_title:           "Ruby: स्ट्रिंग को निचले अक्षर में रूपांतरित करना"
simple_title:         "स्ट्रिंग को निचले अक्षर में रूपांतरित करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyu
Kisi vyakti ko ek string ko lower case mein convert karne mein kyun ruchi hogi, uska maksad 2 vaakyon mein samaanya karna.

## Kaise Kare
Coding ke udaharan aur sample output ke saath "```Ruby ... ```" code blocks ke andar.
```Ruby
string = "HELLO WORLD"
puts string.downcase

# output: hello world
```

## Gehri Jhaank
Ek string ko lower case mein convert karne ka sabse aasan tarika `downcase` method ka upyog karna hai. Is method se hum string ke saare aksharon ko lower case mein tabdeel kar sakte hain aur uska output ek naya string mein prapt kar sakte hain. Yadi hume string ke kisi hisse ko capital letter se lower case mein badalna ho, to hum `gsub` method ka bhi upyog kar sakte hain.

## Dekhein Bhi
"See Also" 
- [Ruby Strings](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby Downcase Method](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ruby Gsub Method](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)