---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Elixir में टेक्स्ट सर्च और रिप्लेस

## क्या और क्यों?
टेक्स्ट सर्चिंग और रिप्लेसिंग का मतलब होता है टेक्स्ट में कुछ विशिष्ट वर्ड्स खोजना और उन्हें अन्य वर्ड्स से बदल देना। प्रोग्रामर्स इसे क्यों करते हैं? डाटा की संरचना बदलने, कस्टम कार्य परिपालन करने, और डाटा सफाई के लिए। 

## कैसे करें:
नीचे Elixir में सर्च और रिप्लेस करने के तरीके का एक उदाहरण है:

```Elixir
str = "नमस्ते, दुनिया!"
IO.puts String.replace(str, "दुनिया", "Elixir")
```

इस कोड का आउटपुट होता है:

```Elixir
"नमस्ते, Elixir!"
```

## गहरी जानकारी
### ऐतिहासिक प्रस्तावना
Elixir में टेक्स्ट सर्च और रिप्लेस की सहुलियत को Erlang भाषा से विरासत में प्राप्त हुआ है, जिसे Elixir का आधार माना जाता है। 

### विकल्प
Elixir में सर्च और रिप्लेस के लिए Regex लाइब्ररी का भी उपयोग किया जा सकता है, जो और अधिक विस्तृत पैटर्न मिलान सुविधाएं प्रदान करता है। 

### कार्यान्वयन विवरण
Elixir में String.replace फ़ंक्शन निम्नलिखित मार्ग के माध्यम से काम करता है:
1. वांछित स्ट्रिंग स्रोत को खोजना 
2. मिलान टेक्स्ट को नये टेक्स्ट से बदलना

## देखें भी
1. [Elixir का आधिकारिक डॉक्यूमेंटेशन](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
2. [Erlang का आधिकारिक डॉक्यूमेंटेशन](https://erlang.org/doc/apps/stdlib/string_processing.html)
3. [Elixir की RegEx मॉड्यूल का डॉक्यूमेंटेशन](https://hexdocs.pm/elixir/1.12/Regex.html)