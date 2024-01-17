---
title:                "स्ट्रिंग को लोअर केस में बदलना"
html_title:           "Elixir: स्ट्रिंग को लोअर केस में बदलना"
simple_title:         "स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

नकल को निचले स्तर (lower case) में रूपांतरित करना एक स्ट्रिंग को छोटा-बड़ा लिखने की प्रक्रिया है। कई प्रोग्रामर्स इसका उपयोग स्ट्रिंग्स को समान तुलना करने और उन्हें डेटा को संपादित करने के लिए करते हैं।

## कैसे करें:

```elixir
string = "Hello, World!"

lowercase_string = String.downcase(string)

IO.puts(lowercase_string)
```

आउटपुट:
```
hello, world!
```

## गहराई में जायें:

निचले स्तर में रूपांतरण का इतिहास है। पहले, कंप्यूटर ऑपरेटिंग सिस्टम्स शिर्षकों को समान तुलना के लिए केस संवर्धन का समर्थन नहीं करते थे। लेकिन आजकल, यह काम प्रोग्रामिंग भाषाओं द्वारा स्वचालित रूप से किया जाता है। अधिक आधुनिक तकनीकी में, पैटर्न (patterns) को मिलान या इनपुट फ़ाइल से पढ़ने के लिए उपयोग किया जाता है। इसके अलावा, कुछ विकल्प भी हैं, जैसे कि सभी अक्षरों को निचले स्तर में संवर्धन करना जो पूर्ण तौर पर अनुकरण के लिए उपयुक्त नहीं है। मानचित्र (mapping) भी संभव है, जहां एक स्ट्रिंग के प्रत्येक अक्षर को एक अलग स्ट्रिंग बनाने के लिए नक्शा किया जाता है।

## इससे जुड़े लिंक:

- [ऑफिशियल Elixir दस्तावेज़ीकरण](https://hexdocs.pm/elixir/String.html#downcase/2)
- [फॉर्मट एक स्ट्रिंग में केस संवर्धन करें](https://stackoverflow.com/questions/433967/how-do-i-convert-a-string-to-lower-case)
- [कोडिंग काल में केस संवर्धन का इतिहास](https://ruleoftech.com/2019/the-history-of-case-conversion-in-computers)