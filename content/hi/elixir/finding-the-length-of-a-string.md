---
title:    "Elixir: एक स्ट्रिंग की लंबाई ढूंढना"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

एक स्ट्रिंग की लंबाई जानने का सबसे साधारण कारण है कि हमें यह पता करना होता है कि उसमें कितने अक्षर हैं। इसके आलावा, कई प्रोग्रामिंग कार्यों में स्ट्रिंगों को इकठ्ठा करने, अलग करने और प्रोसेस करने के लिए भी हमें उनकी लंबाई का पता होना आवश्यक हो सकता है।

## कैसे करें

ईलिक्सिर में स्ट्रिंग की लंबाई जानने के लिए हम `String.length()` फंक्शन का इस्तेमाल कर सकते हैं। नीचे एक उदाहरण दिया गया है:

```
Elixir> String.length("ईलिक्सिर")
5
```

अपने कोड में इस फंक्शन का इस्तेमाल करने के लिए, हमें स्ट्रिंग को वेरिएबल में स्टोर करना होगा और फिर उस वेरिएबल को `String.length()` के साथ पास करना होगा।

## गहराई तक जाएं

स्ट्रिंग की लंबाई का पता लगाना एक एकल ऑपरेशन हो सकता है, लेकिन यह कुछ दूसरे कामों के लिए भी आवश्यक हो सकता है। उदाहरण के लिए, हमें जांच करना हो सकता है कि एक स्ट्रिंग में कितने उपयोगकर्ताओं के नाम हैं और उसे उन्हें अलग करने के लिए उसकी लंबाई का पता होना आवश्यक हो सकता है।

## देखें भी

- [Official Elixir documentation on String module](https://hexdocs.pm/elixir/String.html)
- [Elixir tutorial on strings](https://elixir-lang.org/getting-started/string-interpolation.html)
- [Elixir string functions](https://www.tutorialspoint.com/elixir/elixir_strings.htm)
- [Understanding string length in programming](https://www.geeksforgeeks.org/find-length-of-a-string-in-elixir/)