---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- hi/elixir/using-regular-expressions.md
date:                  2024-02-03T19:17:16.374111-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

एलिक्सिर में नियमित अभिव्यक्तियाँ (regex) विशिष्ट पैटर्न के आधार पर स्ट्रिंग्स की खोज, मिलान, और हेरफेर के लिए प्रयोग की जाती हैं। प्रोग्रामर स्ट्रिंग हैंडलिंग में इसकी कुशलता और बहुउपयोगिता का लाभ उठाते हैं, जैसे कि प्रारूपों का सत्यापन (ईमेल, यूआरएल), लॉग्स या डेटा निष्कर्षण के लिए।

## कैसे:

एलिक्सिर `Regex` मॉड्यूल का उपयोग करता है, जो एर्लैंग की regex लाइब्रेरी का लाभ उठाता है, regex ऑपरेशनों के लिए। यहाँ मूल उपयोग हैं:

```elixir
# एक पैटर्न का मिलान - पहले मैच को लौटाता है
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # आउटपुट: ["hello"]

# सभी मैचों को ढूँढना
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # आउटपुट: [["2"], ["5"]]

# एक स्ट्रिंग के हिस्सों को बदलना
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # आउटपुट: "Elixir_is_fun"
```

अधिक जटिल पैटर्न और कार्यक्षमताओं के लिए, आप तृतीय-पक्ष पुस्तकालयों का उपयोग करने पर विचार कर सकते हैं, हालाँकि अधिकांश मुख्य स्ट्रिंग और पैटर्न मिलान कार्यों के लिए, एलिक्सिर का निर्मित `Regex` मॉड्यूल काफी शक्तिशाली है।

एक केस-असंवेदनशील मैच निष्पादित करने के लिए, `i` विकल्प का उपयोग करें:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # आउटपुट: ["Hello"]
```

कुशलता के लिए, Regex अभिव्यक्तियों को बार-बार उपयोग किए जाने पर पूर्व-संकलित किया जा सकता है:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # आउटपुट: ["hello"]
```

एलिक्सिर नामित कैप्चर्स का समर्थन भी करता है, जो एक स्ट्रिंग के विशिष्ट भागों को निकालने के लिए बहुत उपयोगी हो सकता है, जबकि आपके कोड को अधिक पठनीय बनाता है:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # आउटपुट: %{"year" => "2023", "month" => "04", "day" => "15"}
```

यह संक्षिप्त अवलोकन एलिक्सिर द्वारा नियमित अभिव्यक्तियों से निपटने की सुविधा को रेखांकित करता है, जिससे शक्तिशाली स्ट्रिंग हेरफेर और डेटा निष्कर्षण तकनीकों को सक्षम किया जा सकता है।
