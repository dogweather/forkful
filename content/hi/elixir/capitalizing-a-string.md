---
title:                "एक स्ट्रिंग को बड़े अक्षरों में बदलना"
html_title:           "Elixir: एक स्ट्रिंग को बड़े अक्षरों में बदलना"
simple_title:         "एक स्ट्रिंग को बड़े अक्षरों में बदलना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग को कैपिटलाइज करना मतलब होता है उसका पहला अक्षर बड़ा (यानी अपरकेस) करना। किसी भी केस को यानी लोअरकेस को अपरकेस में बदलने के लिए प्रोग्रामर इसे उपयोग करते हैं, यह उपयोगी होता हैं जब उन्हें किसी शब्द को स्पष्ट रूप से दर्शाने की आवश्यकता होती है।

## कैसे करें:
Elixir में, आप `String.capitalize/2` फ़ंक्शन का उपयोग करके स्ट्रिंग के पहले अक्षर को कैपिटलाइज कर सकते हैं। जैसे की:

```Elixir
IO.puts String.capitalize("namaste duniya")
```
आउटपुट होता है:
```
Namaste duniya
```

## गहरी जानकारी

कैपिटलाइजेशन का आविष्कार संचार के उद्देश्यों को और अधिक प्रभावी तरीके से पूरा करने के लिए हुआ। इसे उपयोग में लेने से शब्दों और वाक्यांशों का स्पष्टीकरण करना आसान होता है। 

Elixir में `String.upcase/1` जैसे विकल्प भी हैं, जो स्ट्रिंग के सभी अक्षरों को अपरकेस में बदल देता है, लेकिन यदि आप केवल पहले अक्षर को बदलना चाहते हैं, तो `String.capitalize/2` शायद बेहतर हो सकता है। 

`String.capitalize/2` फ़ंक्शन Elixir के `String` मॉड्यूल का हिस्सा है, जो UTF-8 संकोच को समर्थन करता है। इसका मतलब है कि यह Unicode आधारित भाषाओं के साथ काम करने में सक्षम है।

## और भी देखें:
   
String.capitalize/2 - [https://hexdocs.pm/elixir/String.html#capitalize/2](https://hexdocs.pm/elixir/String.html#capitalize/2)

String.upcase/1 - [https://hexdocs.pm/elixir/String.html#upcase/1](https://hexdocs.pm/elixir/String.html#upcase/1)