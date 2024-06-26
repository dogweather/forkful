---
date: 2024-01-26 03:40:30.686583-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Elixir \u092E\u0947\
  \u0902 \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 '\u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\
  \u0947' \u0915\u0940 \u0915\u093E\u0930\u094D\u092F\u0915\u094D\u0937\u092E\u0924\
  \u093E \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u092A\
  \u0948\u091F\u0930\u094D\u0928 \u092E\u093F\u0932\u093E\u0928 \u092F\u093E `String`\
  \ \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947 \u0905\u092A\u0928\u0940 \u0938\u094D\u0935\u092F\
  \u0902 \u0915\u0940 \u092C\u0928\u093E\u0928\u093E\u2026"
lastmod: '2024-03-13T22:44:51.715594-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u092E\u0947\u0902 \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\u092E\
  \u093F\u0924 '\u0909\u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928\
  \ \u0939\u091F\u093E\u0928\u0947' \u0915\u0940 \u0915\u093E\u0930\u094D\u092F\u0915\
  \u094D\u0937\u092E\u0924\u093E \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\
  \u0915\u093F\u0928 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u093F\u0932\u093E\
  \u0928 \u092F\u093E `String` \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0905\u092A\u0928\u0940\
  \ \u0938\u094D\u0935\u092F\u0902 \u0915\u0940 \u092C\u0928\u093E\u0928\u093E \u092C\
  \u0939\u0941\u0924 \u0906\u0938\u093E\u0928 \u0939\u0948\u0964 \u0907\u0928 \u0928\
  \u092E\u0942\u0928\u094B\u0902 \u0915\u094B \u0926\u0947\u0916\u0947\u0902."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\
  \u093E"
weight: 9
---

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
