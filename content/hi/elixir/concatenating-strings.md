---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग को जोड़ने का तात्पर्य होता है कि दो या दो से अधिक स्ट्रिंग्स को एकसाथ जोड़ना। प्रोग्रामर्स इसे करते हैं ताकि वे डाटा को तब्दील कर सकें और यह सुनिश्चित कर सकें कि वे परिणामस्वरूप चाहे वह जो भी हो, इसे उपयोगकर्ता को प्रस्तुत कर सकें। 

## कैसे करें: (How to)

Elixir में, आप ```<>``` ऑपरेटर का उपयोग करके स्ट्रिंग्स को जोड़ सकते हैं। 

```elixir
x = "नमस्ते"
y = "दुनिया"
message = x <> ", " <> y
IO.puts message
```

यह कोड "नमस्ते, दुनिया" दिखाएगा।

## गहराी में (Deep Dive)

Elixir में string concatenation के लिए ```<>``` ओपरेटर का इस्तेमाल करना एलिक्सिर संघों के बीच आमतौर पर स्वीकार किया जाता है। इतिहासिक दृष्टिकोण से, स्ट्रिंग कंकेटनेशन को विभिन्न प्रोग्रामिंग भाषाओं में विभिन्न तरीके से हाथलिया जा सकता है। 

वास्तव में, एलिक्सिर में भी `String.concat/2` का एक विकल्प है, लेकिन इसे आमतौर पर ```<>``` के लिए एक विकल्प के रूप में नहीं देखा जाता है। 

```elixir
x = "नमस्ते"
y = "दुनिया"
message = String.concat([x, ", ", y])
IO.puts message
```
आपका स्ट्रिंग कंकेटनेशन कोई भी हो, यह बहुत महत्वपूर्ण है कि आप सुनिश्चित करें कि सभी स्ट्रिंग्स UTF-8 encoded होने चाहिए। 

## अधिक देखें (See Also)

* [नियमित अभिव्यक्तियों से जुड़े सूत्रों का उपयोग करके स्ट्रिंग्स मिलाना](https://elixirschool.com/en/lessons/advanced/regular-expressions/)
* [Elixir डॉक्स में स्ट्रिंग कंकटिनेशन](https://hexdocs.pm/elixir/String.html#module-concatenation)
* [Elixir में I/O और स्ट्रिंग्स का उपयोग](https://elixir-lang.org/getting-started/io-and-the-file-system.html)