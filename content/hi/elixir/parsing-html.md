---
title:                "HTML को विश्लेषण करना"
html_title:           "Elixir: HTML को विश्लेषण करना"
simple_title:         "HTML को विश्लेषण करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

 Web से डेटा को एक डेटा हस्तक्षेप के रूप में आपस में जोड़ने की आवश्यकता के लिए आप कभी-कभी HTML को अन्यता मानचित्रण की आवश्यकता होती है।
 
## कैसे

```elixir
html = "<div><p>Hello, World!</p></div>"
parsed_html = Floki.parse(html)
title = Floki.find(parsed_html, "div p")
IO.puts title
```

इस उदाहरण के साथ, हमने HTML से टैग `<div><p>Hello, World!</p></div>` को ` parsed_html` में संग्रहीत करके HTML-डोम पर संगठन और औसत भाषा उपनिषद को प्राप्त किया। हमने परिणाम के रूप में "Hello, World!" छपाया।

## गहराई में जाएं

HTML भाषा अत्यधिक विस्तृत और उपयोगी हो सकती है, और Elixir में पार्सिंग HTML करने के साथ-साथ आपको अन्य पाठ्यकार निर्माण वस्तुओं, विचारों, और पारिस्थितिकी धारणाओं के बारे में सीख सकते हैं।

## देखिये भी

- [Elixir डोम के रूप में HTML पार्स करने का विस्तृत ट्यूटोरियल](https://hexdocs.pm/floki/api-reference.html)
- [Elixir भाषा का मूल ज्ञान प्राप्त करें](https://elixir-lang.org/getting-started/introduction.html)
- [HTML भाषा का संस्करण-सिरोहित शिक्षण प्रणाली](https://www.w3schools.com/html/)