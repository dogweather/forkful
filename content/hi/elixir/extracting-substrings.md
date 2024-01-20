---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

सबस्ट्रिंग निकालना यानी एक बड़ी स्ट्रिंग से छोटी-छोटी स्ट्रिंग बनाना होता है। यह काम करना इसलिए जरूरी होता है जब प्रोग्रामर्स को डाटा को खास तरीके से प्रमुखित करना, संग्रहीत करना या अनुकरण करना होता है। 

## कैसे:

एलिक्सिर में सबस्ट्रिंग को निकालने के उदाहरण और आउटपुट निचे दिए गए हैं।

```elixir
defmodule Main do
 def run do
    original_string = "नमस्ते, दुनिया!"
    substring = String.slice(original_string, 0, 6)
    IO.puts substring
 end
end

Main.run

# Output - नमस्ते
```

यह कोड "नमस्ते, दुनिया!" स्ट्रिंग को "नमस्ते" में बदलता है।

## गहराई में:

1. ऐतिहासिक संदर्भ: पैराडाइग्म या मॉडल की खोज करने से पहले प्रोग्रामर्स को अक्सर स्ट्रिंग का प्रयोग करना पड़ता था।
2. विकल्प: अन्य भाषाओं, जैसे की JavaScript, में slice() फ़ंक्शन का उपयोग किया जाता है।
3. कार्यान्वयन विवरण: String.slice/3 Elixir फ़ंक्शन में कस्टमाइज़ेशन की अनुमति होती है, जैसे कि उपयोगकर्ता निर्दिष्ट कर सकते हैं कि वे कौन सी स्थिति से शुरू होना चाहते हैं।

## और भी देखें:

1. Elixir का आधिकारिक दस्तावेज़ीकरण: https://elixir-lang.org/docs.html
2. Guru99 का ट्यूटोरियल 'Elixir String Functions' हेल्पफुल हो सकता है। (https://www.guru99.com/elixir-string-functions.html)