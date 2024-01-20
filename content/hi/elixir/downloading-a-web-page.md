---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेबपेज को डाउनलोड करना मतलबे उसे अपने कंप्यूटर पर स्थानीय तौर पर सहेजना। प्रोग्रामर्स इसे सॉर्स कोड को अनालाइज़ या सामग्री का परीक्षण करने के लिए करते हैं। 

## कैसे करें:

```Elixir
defmodule Downloader do
  def download_page(url) do
    {:ok, body} = HTTPoison.get!(url).body
    String.split(body, "\n")
  end
end
```

फ़ंक्शन `download_page` यूआरएल को डाउनलोड करता है और वेबपेज की सामग्री को वापस लौटाता है। ये सामग्री फाइल्स की फॉर्म में होती हैं।

## ज्यादा ज़रूरी जानकारी

वेबपेजों की डाउनलोडिंग हमेशा से ही जरूरी रही है, चाहे वे ऑफ़लाइन पढ़ने के लिए हों या डेटा एनालिटिक्स परोजेक्ट्स के लिए। इतने सालों में कई दूसरे तरीके भी आ गए हैं, जैसे कि `wget` या `curl` उपयोग करना लेकिन प्रोग्राम पुरा नियंत्रण देता है कि किसे डाउनलोड करना हैं और उसे कैसे संसाधित करना हैं। 

यदि आप Elixir में डाउनलोडिंग करना चुनते हैं, तो आप `HTTPoison` जैसे पैकेज पर भरोसा कर सकते हैं, जो एक HTTP प्रोटोकॉल पुस्तकालय है।

## देखें भी:

1. [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
2. [Elixir Official Website](https://elixir-lang.org/)
3. [Hex: package manager for the Erlang ecosystem](https://hex.pm/) 
4. [Elixir School](https://elixirschool.com/en/)