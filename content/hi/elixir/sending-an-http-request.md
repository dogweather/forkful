---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना, वेब सर्वर को किसी विशेष वेब पृष्ठ के लिए कहना होता है। प्रोग्रामर्स आमतौर पर डाटा त्रुटियों और सर्वर रेस्पॉन्स का विश्लेषण करने के लिए इसे करते हैं।

## कैसे कরें:

Elixir का HTTPoison लाइब्रेरी HTTP अनुरोध भेजने में मदद कर सकता है। पहले इसे अपने प्रोजेक्ट में जोड़ें:

```elixir
def deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

फिर, आप एक अनुरोध भेज सकते हैं:

```elixir
HTTPoison.get!("https://www.example.com").body
```

यह आपको वेबसाइट का सामग्री दिखाएगा।

## गहरा डाइव:

Elixir 2011 में एरिक स्टेनवाल्ड द्वारा डिजाइन की गई थी और इसे जाविट्स के एफएम. HTTPoison, Elixir का एक पैकेज है, जो HTTP प्रोटोकॉल का उपयोग करके डेटा भेजता है। इसे वयस्कर्ण की सहायता से निर्मित किया गया है, जो एक खुले स्रोत और पूरी तरह से विस्तारणीय Elixir HTTP library है।

इसके विकल्प में Finch, Tesla और Mint शामिल हैं। Finch और Tesla में प्रगति की जांच, विभिन्न बैकएंड्स का समर्थन, गबनावय समर्थन आदि उच्च स्तर की सुविधाएं होती हैं। जबकि Mint, कम स्तरीय, अधिक सरल एपीआई से काम करने की अनुमति देता है।

रिक्वेस्ट्स और रेस्पॉन्सेस विभाजित शक्तिशाली होते हैं, जिसके परिणामस्वरूप आपके एप्प्लीकेशन में प्रकाशनोत्तर को प्रबंधित करने में मदद मिलती है।

## देखें भी:
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Tesla GitHub](https://github.com/teamon/tesla)
- [Finch GitHub](https://github.com/keathley/finch)
- [Mint GitHub](https://github.com/elixir-mint/mint)
- [Elixir की आधिकारिक वेबसाइट](https://elixir-lang.org/)