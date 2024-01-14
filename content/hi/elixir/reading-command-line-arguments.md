---
title:                "Elixir: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Hindi Translation:

## क्यों

क्या आप अपने बहुत सारे एलिक्सिर प्रोग्राम को स्क्रिप्ट रूप में चलाने को चाहते हैं? या शायद आप एलिक्सिर में शामिल किए गए विशिष्ट विशेषताओं को टेस्ट करना चाहते हैं? तो "कमांड लाइन आर्ग्यूमेंट्स" को पढ़ना आपके लिए बहुत ही महत्वपूर्ण हो सकता है। इस ब्लॉग पोस्ट में, हम आपको बताएंगे कि आप कैसे आसानी से एलिक्सिर में कमांड लाइन आर्ग्यूमेंट्स को पढ़ सकते हैं और इससे अपने कोड को मजबूत और उपयोगी बना सकते हैं।

## कैसे करें

एलिक्सिर में कमांड लाइन आर्ग्यूमेंट्स पढ़ना बहुत ही आसान है। सबसे पहले, आपको "Command" मॉड्यूल को इम्पोर्ट करना होगा। फिर आप "Args" क्लास का इंस्टेंस बना सकते हैं। इसके बाद, आप "parse" फंक्शन का उपयोग करके स्ट्रिंग से नए आर्ग्यूमेंट्स की रिस्ट बना सकते हैं। नीचे दिए गए कोड ब्लॉक में हमने एक उदाहरण दिया है जिसमें हम स्क्रिप्ट को चलाने के लिए नाम, उम्र और प्रोफेशन का आर्ग्यूमेंट पार्स करते हैं। आप इस उदाहरण को अपने कोड में भी शामिल कर सकते हैं।

```Elixir
defmodule Example do
   def main(args) do
      # Create an Args instance
      arg = Args.parse(args)

      # Get the value of "name" argument
      name = Args.get(arg, "name")

      # Get the value of "age" argument
      age = Args.get(arg, "age")

      # Get the value of "profession" argument
      profession = Args.get(arg, "profession")

      # Print out the values
      IO.puts "Name: #{name}"
      IO.puts "Age: #{age}"
      IO.puts "Profession: #{profession}"
   end
end
```

आप कोड को रन करने के ब