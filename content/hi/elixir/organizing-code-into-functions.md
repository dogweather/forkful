---
title:                "कोड को फंक्शन्स में व्यवस्थित करना"
date:                  2024-01-26T01:10:47.006197-07:00
model:                 gpt-4-1106-preview
simple_title:         "कोड को फंक्शन्स में व्यवस्थित करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोड को फंक्शन्स में आयोजित करने का तात्पर्य संबंधित कार्यों को पुन: प्रयोज्य ब्लॉक्स में विभाजित करना है। हम इसे पठनीयता और बनाए रखने की क्षमता में सुधार, दोहराव को कम करने, और परीक्षण को सरल बनाने के लिए करते हैं।

## कैसे करें:
आइए Elixir में एक सरल फंक्शन बनाएँ जो शब्दों को बड़ा अक्षर में बदल सकता है:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
आउटपुट:
```
Hello Elixir World
```
यहाँ, हमने शब्दों को बड़ा अक्षर में बदलने के तर्क को नीटली `capitalize_words` नामक फंक्शन में पैकेज किया है।

## गहराई से अध्ययन
Elixir में, और व्यापक Erlang VM ईकोसिस्टम में, फंक्शन्स पहले क्लास के नागरिक होते हैं, समस्याओं को छोटे, संभालने योग्य, और पृथक टुकड़ों में नीचे तोड़ने के दर्शन को अपनाते हैं। इतिहासिक रूप से, इस फंक्शनल दृष्टिकोण की जड़ें लैम्ब्डा कैलकुलस और लिस्प्स में हैं, जो कोड को डेटा के रूप में दर्शाने वाले दर्शन को बढ़ावा देते हैं।

कोड को आयोजित करने के विकल्प में Elixir में मैक्रो या प्रोसेस का उपयोग क्रमशः दोहरावदार या समवर्ती कार्यों के लिए हो सकता है। कार्यान्वयन के दृष्टिकोण से, Elixir फंक्शन्स पैटर्न मैचिंग को संभाल सकते हैं और अलग-अलग तर्क (ऐरिटी) प्राप्त कर सकते हैं, जो उन्हें बहुमुखी प्रदान करते हैं।

## देखें भी
- [फंक्शन्स पर Elixir का आधिकारिक दस्तावेज़](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas की "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)