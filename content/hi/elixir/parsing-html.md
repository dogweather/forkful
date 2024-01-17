---
title:                "HTML को विभाजन करना"
html_title:           "Elixir: HTML को विभाजन करना"
simple_title:         "HTML को विभाजन करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग क्या है और क्यों कई प्रोग्रामर इसे करते हैं इसके बारे में जानने के लिए इस आलेख को पढ़ें।

## कैसे करें:
इलिक्सर में HTML पार्सिंग के कुछ उदाहरण और उसका आउटपुट नीचे दिए गए कोड ब्लॉक में दिए गए है।

```elixir
# input HTML string
html_string = "<p>Hello World!</p>"

# parsing HTML using Floki library
html_parsed = Floki.parse(html_string)

# accessing the parsed elements
Floki.find(html_parsed, "p") |> Floki.text()
```

आउटपुट:
"Hello World!"

## गहराई तक:
ऐतिहासिक परिस्थितियों, विकल्प और HTML पार्सिंग के बारे में कुछ महत्वपूर्ण जानकारियां भी हमारे पास हैं। HTML पार्सिंग या एम्बीडेड कोड को प्रोसेस करने के लिए अन्य विकल्पों के समान, इलिक्सर भी इसके लिए आसान और अनुकूल इलिक्सर प्रोग्रामिंग भाषा है। HTML पार्सिंग का एक पात्र और निराशाजनक उदाहरण हमारे वेब ब्राउजर है, जो हर HTML पेज को पार्स करता है और आपको प्रदर्शित करता है।

## आगे देखें:
अगर आपको HTML पार्सिंग समझने में और और जानकारी चाहिए तो आप इन लिंक्स को देख सकते हैं:

1. [Elixir के ऑफिशियल डॉक्यूमेंटेशन](https://elixir-lang.org/getting-started/introduction.html)
2. [Floki लाइब्रेरी की वेबसाइट](https://hexdocs.pm/floki/api-reference.html)
3. [HTML पार्सिंग के विभिन्न तरीके](https://dzone.com/articles/parsing-html-using-elixir)
4. [Elixir के बारे में और अधिक जानकारी](https://www.tutorialspoint.com/elixir/index.htm)