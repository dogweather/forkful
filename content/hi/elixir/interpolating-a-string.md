---
title:                "स्ट्रिंग को भमिमित करना"
html_title:           "Elixir: स्ट्रिंग को भमिमित करना"
simple_title:         "स्ट्रिंग को भमिमित करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

"What & Why?": String interpolation क्या है और प्रोग्रामर्स इसे क्यों करते हैं?

String interpolation का मतलब होता है, एक स्ट्रिंग के भीतर एक वैरिएबल या एक्सप्रेशन को प्रयोग करने से, हम उसी स्ट्रिंग के अंदर के भागों को परिवर्तित कर सकते हैं। यह हमें एक स्ट्रिंग बनाने के लिए अलग-अलग भागों को कनकट करने वाले धीरे-धीरे कोडिंग से बचाता है।

हम इसका प्रयोग करते हैं क्योंकि यह हमें अपने कोड में स्ट्रिंग्स को अधिक दक्षता से प्रयोग करने की सुविधा देता है। इससे हमें एक स्ट्रिंग को बनाने के लिए ज्यादा से ज्यादा कोड नहीं लिखना पड़ता है और हमारा कोड भी आसान और स्पष्ट बनता है।

"How to:":

```Elixir
name = "John"
age = 30
IO.puts" My name is #{name} and I am #{age} years old."
```

आउटपुट:
 My name is John and I am 30 years old.

"Deep Dive":
गहराई से जाने:

स्ट्रिंग इंटरपोलेशन का आविष्कार 1979 में पास्कल के भाषण बॉयर्स द्वारा किया गया था और यह प्रोग्रामिंग भाषाओं में से कई में उपयोगी है। आमतौर पर यह अन्य विकल्पों के मुकाबले अधिक दक्षता और सुविधाएं प्रदान करता है। स्ट्रिंग कोडिंग के साथ-साथ एक स्ट्रिंग इंटरपोलेशन के उदाहरण के लिए, यह मूल एक्सप्रेशन से अधिक सुविधा प्रदान करता है, जो पहले से ही पाठ को रखता है और उसे अधिक नाश्तादेह कोडिंग से बचाता है।

"See Also":
संबंधित स्रोतों के लिंक: 
- एलिक्सियर ऑफिसियल डॉक्यूमेंटेशन (https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%23:%7Binterpolation%7D/2)
- स्ट्रिंग इंटरपोलेशन के लिए और अन्य विकल्पों के लिए, देखें: एलिक्सियर का ब्लॉगपोस्ट (https://elixir-lang.org/blog/2017/01/05/string-to-some-more/)