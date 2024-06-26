---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:16.374111-07:00
description: "\u0915\u0948\u0938\u0947: \u090F\u0932\u093F\u0915\u094D\u0938\u093F\
  \u0930 `Regex` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u094B \u090F\u0930\
  \u094D\u0932\u0948\u0902\u0917 \u0915\u0940 regex \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940 \u0915\u093E \u0932\u093E\u092D \u0909\u0920\u093E\u0924\
  \u093E \u0939\u0948, regex \u0911\u092A\u0930\u0947\u0936\u0928\u094B\u0902 \u0915\
  \u0947 \u0932\u093F\u090F\u0964 \u092F\u0939\u093E\u0901 \u092E\u0942\u0932 \u0909\
  \u092A\u092F\u094B\u0917 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:51.718991-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0932\u093F\u0915\u094D\u0938\u093F\u0930 `Regex` \u092E\u0949\u0921\
  \u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0924\u093E \u0939\u0948, \u091C\u094B \u090F\u0930\u094D\u0932\u0948\u0902\u0917\
  \ \u0915\u0940 regex \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\
  \u093E \u0932\u093E\u092D \u0909\u0920\u093E\u0924\u093E \u0939\u0948, regex \u0911\
  \u092A\u0930\u0947\u0936\u0928\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F\u0964\
  \ \u092F\u0939\u093E\u0901 \u092E\u0942\u0932 \u0909\u092A\u092F\u094B\u0917 \u0939\
  \u0948\u0902."
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
weight: 11
---

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
