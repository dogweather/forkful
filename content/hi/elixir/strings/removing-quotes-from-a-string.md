---
title:                "स्ट्रिंग से उद्धरण चिह्न हटाना"
date:                  2024-01-26T03:40:30.686583-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से उद्धरण चिह्न हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से उद्धरण चिह्नों (quotes) को हटाना का मतलब है उन अतिरिक्त लपेटों को दूर करना जिससे भीतर का शुद्ध पाठ मिल सके। प्रोग्रामर इसे इनपुट को सेनिटाइज़ करने, त्रुटियों से बचने, और उन स्थितियों के लिए डेटा की तैयारी के लिए करते हैं जहाँ उद्धरण चिह्न सुविधा नहीं बल्कि बाधा होते हैं।

## कैसे करें:
Elixir में कोई निर्मित 'उद्धरण चिह्न हटाने' की कार्यक्षमता नहीं है, लेकिन पैटर्न मिलान या `String` कार्यों का उपयोग करके अपनी स्वयं की बनाना बहुत आसान है। इन नमूनों को देखें:

```elixir
# पैटर्न मिलान का उपयोग करते हुए
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# नमूना उपयोग
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# String.trim/1 का उपयोग करते हुए
def unquote_string(string), do: String.trim(string, "'\"")

# नमूना उपयोग
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

दोनों विधियों का आउटपुट होगा:
```
"Hello, World!"
```

## गहराई से विचार
पहले के समय में, स्ट्रिंग्स में उद्धरण एक खाई की तरह होते थे—उन्हें संभालने में गलती हो जाए, तो बूम, सिंटैक्स त्रुटियाँ या सुरक्षा संबंधित छिद्र हो जाते थे। Elixir में, पैटर्न मिलान आपके स्ट्रिंग्स को लेगो ब्लॉक्स की तरह मानता है, जिससे आप बारीकी से उन्हें अलग कर सकें और पुनः बना सकें। इसका रोबस्ट `String` मॉड्यूल भी काम आता है, लचीले ढंग से `trim` कार्यों के साथ उद्धरण को हटा देता है। विकल्प? नियमित अभिव्यक्तियाँ (regular expressions) उद्धरणों को दूर कर सकती हैं, और बाहरी पुस्तकालय अगर आपको बुनियादी हटाव से अधिक की आवश्यकता हो तो अतिरिक्त शक्ति प्रदान कर सकते हैं।

## देखें
इनके साथ और गहराई में जाएँ:
- [Elixir का String मॉड्यूल](https://hexdocs.pm/elixir/String.html)
- [Elixir में पैटर्न मिलान के बारे में जानें](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixir में नियमित अभिव्यक्तियाँ (Regex मॉड्यूल)](https://hexdocs.pm/elixir/Regex.html)
