---
title:    "Elixir: एक अस्थायी फ़ाइल बनाना"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# क्यों
अगर आप Elixir प्रोग्रामिंग में नए हो और उसके साथ जुड़ा हैं, तो आपने शायद कभी temporary file बनाने की बात सुनी होगी। लेकिन आखिर ऐसा क्यों करना होता है? आइए जानते हैं!

# कैसे
पहले हम देखेंगे कि Elixir में temporary file कैसे बनाएं। चलिए एक simple example के साथ शुरू करते हैं:

```Elixir
file = File.tmp!()
```

यह कोड हमें एक temporary file के लिए पथ देगा जिसे हम प्रोग्राम में उपयोग कर सकते हैं। इसके अलावा, हम फाइल को उपयोग करके उसे delete भी कर सकते हैं।

```Elixir
File.delete(file)
```

अगर आप temporary file को कुछ specific नाम देना चाहते हैं, तो निम्न तरीके से कर सकते हैं:

```Elixir
{path, descriptor} = File.temp!(["prefix-", ".txt"])
```

यहाँ "prefix-" फ़ाइल का शुरुआती नाम और ".txt" उसका एक्सटेंशन है।

# गहराई में जाने
Temporary file बनाने की विस्तृत जानकारी के लिए आप File लाइब्रेरी को देख सकते हैं। यहाँ कुछ लिंक हैं जो आपको temporary file बनाने के बारे में और भी बेहतर समझने में मदद कर सकते हैं:

- [File लाइब्रेरी डॉक्यूमेंटेशन](https://hexdocs.pm/elixir/File.html#tmp%21/1)
- [File लाइब्रेरी कोड इक्षन्जाम्पल्स](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/file/test/file_test.exs)

# देखें भी
- [Elixir क्या है?](https://geekflare.com/what-is-elixir/)
- [Elixir वार्निंग: कैसे उन्हें हैंडल करें](https://www.phoenixframework.org/blog/how-to-handle-elixir-warnings)

# उम्मीद है कि यह आपको मदद करेगा temporary file बनाने में! अपने कीबोर्ड कार्यालय में यह तस्वीर डालकर कॉपी पेस्ट कीजिए और अपने codi

आशा है क