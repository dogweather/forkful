---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

किसी पैटर्न से मिलते अक्षरों को हटाना एक कॉडिंग प्रासंगिक क्रिया है जिसमें, एक विशिष्ट पैटर्न या शृंखला से मेल खाने वाले characters को हटाया जाता है। कार्यक्रमकर्ताओं इसे करने की आवश्यकता तब होती है जब उन्हें रिकॉर्डबार डाटा से कुछ विशेष वर्ण हटाना होता है, जैसे की नकरात्मक चिन्ह या अनुपयोगी व्हाइटस्पेस।

## कैसे करें:

Elixir में String.replace/3 function का उपयोग करके पैटर्न से मिलने वाले characters को हटाया जा सकता है।

```Elixir
defmodule Example do
  def remove_matching_chars(str, pattern) do
    String.replace(str, pattern, "")
  end
end

iex> Example.remove_matching_chars("नमस्ते, दुनिया!", "न")
"मस्ते, दुिया!"
```
इसमें, हमने "न" अक्षर को हटाया है।

## डीप डाइव:

इस तकनीक का इस्तेमाल करके, हम एक string के अनुपयोगी characters को हटा सकते हैं। हालांकि, यह लगातार बदल रहे डाटा के साथ कुछ अश्वस्त कर सकता है क्योंकि प्रत्येक बार जब डाटा बदलता है, हमें पैटर्न को अपडेट करने की आवश्यकता हो सकती है। 

इसके विकल्प के रूप में, हम रेगेक्स (Regular Expressions) का उपयोग कर सकते हैं जो हमें व्यापक शृंखलाएँ तैयार करने की क्षमता देती हैं।

## देखने के लिए:

- String.replace/3: https://hexdocs.pm/elixir/String.html#replace/3
- Regular Expressions in Elixir: https://elixir-lang.org/getting-started/regex.html