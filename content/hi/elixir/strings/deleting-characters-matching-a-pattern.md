---
date: 2024-01-20 17:42:18.370924-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elixir\
  \ \u092E\u0947\u0902, `String.replace/4` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\
  \u093E \u092A\u094D\u0930\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0939\u092E\
  \ \u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u0947 \u092E\u093F\u0932\u0928\u0947\
  \ \u0935\u093E\u0932\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B\
  \ \u0939\u091F\u093E \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:53.722428-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elixir \u092E\u0947\
  \u0902, `String.replace/4` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u092A\
  \u094D\u0930\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0939\u092E \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u0947 \u092E\u093F\u0932\u0928\u0947 \u0935\u093E\
  \u0932\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\u091F\
  \u093E \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
Elixir में, `String.replace/4` फंक्शन का प्रयोग करके हम पैटर्न के मिलने वाले अक्षरों को हटा सकते हैं:

```elixir
# एक सिंपल स्ट्रिंग:
original_text = "Hello, 123 World! 456"

# डिजिट्स हटाएं:
cleaned_text = String.replace(original_text, ~r/\d/, "")
IO.puts(cleaned_text)
```

जब ऊपर का कोड चलेगा, आउटपुट होगा:
```
Hello,  World! 
```

इसी प्रकार, हम और भी जटिल पैटर्न बना सकते हैं:

```elixir
# छोटे और बड़े अक्षर H और W हटाएं:
cleaned_text = String.replace(original_text, ~r/[HhWw]/, "")
IO.puts(cleaned_text)
```

आउटपुट होगा:
```
ello, 123 orld! 456
```

## Deep Dive (गहराई से जानकारी)
Elixir में टेक्स्ट से अक्षर हटाने के लिए `String` मॉड्यूल का इस्तेमाल किया जाता है। `String.replace/4` फंक्शन का इस्तेमाल कर के हम रेग्युलर एक्सप्रेशन्स (regular expressions) की मदद से किसी खास पैटर्न से मेल खाने वाले अक्षरों को हटा सकते हैं। यह काम इसलिए करते हैं क्योंकि अक्सर हमें डेटा का एक विशेष तरीके से प्रोसेसिंग करने की आवश्यकता होती है - जैसे की यूज़र इनपुट से स्पेशल कैरेक्टर्स को हटाना या लॉग्स को पार्स करना. इतिहास में, रेग्युलर एक्सप्रेशन का विकास टेक्स्ट को संसाधित करने के लिए किया गया था और Elixir इस पारंपरिक उपकरण को बहुत ही कुशलता से प्रयोग करती है।

वैकल्पिक रूप से, यदि पैटर्न सिंपल हो, तो `String.replace/4` के बजाय `String.trim/2`, `String.slice/3` या अन्य `String` मॉड्यूल फंक्शन्स का उपयोग कर सकते हैं। यह सब एप्रोचेज काम के हिसाब से अच्छी हो सकती हैं।

## See Also (अन्य सूत्रों के लिए):
- Elixir के आधिकारिक डॉक्युमेंटेशन में `String` मॉड्यूल के बारे में जानें: [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- रेग्युलर एक्सप्रेशन के बुनियादी ज्ञान के लिए: [Regular Expressions](https://www.regular-expressions.info/)
- Elixir के पैटर्न मैचिंग के बारे में अधिक जानने के लिए: [Elixir Pattern Matching](https://elixir-lang.org/getting-started/pattern-matching.html)
