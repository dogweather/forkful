---
title:                "Elixir: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों 

वेब पृष्ठ को डाउनलोड करने का कोई महत्व है क्योंकि यह आपको विशेष जानकारी और संसाधनों तक पहुंच प्रदान कर सकता है। इसके अलावा, आप वेब पृष्ठ के संदर्भ में रहते हैं और इनके साथ काम करते हो तो आपको इसके बारे में अधिक जानने की आवश्यकता हो सकती है।

## कैसे करें

यदि आप वेब पृष्ठ को डाउनलोड करना चाहते हैं, तो आपको इसके लिए एक क्रॉलर या स्क्रेपर की आवश्यकता होगी। इसमें आप एक Elixir प्रोग्रामिंग भाषा का उपयोग करके काम कर सकते हैं। नीचे दिए गए कोड सम्पल के माध्यम से आप डाउनलोड कर सकते हैं:

```Elixir
defmodule WebScraper do
  def download_page(url) do
    %{status_code: code, body: body} = HTTPoison.get(url)
    IO.puts "Status code: #{code}"
    IO.puts "Body: #{body}"
  end
end

WebScraper.download_page("https://www.example.com")
```

आउटपुट:

```
Status code: 200
Body: <html><head>...</head><body>...</body></html>
```

## गहराई में जाएं

अपने Elixir कोड के साथ, आप विभिन्न मॉड्यूल और लाइब्रेरी का उपयोग करके वेब पृष्ठों को डाउनलोड करने में मदद के लिए विस्तृत जानकारी भी प्राप्त कर सकते हैं। उदाहरण के लिए, आप HTTPoison, Floki, या Hackney जैसी पहले से बनी हुई Elixir पैकेजों का उपयोग कर सकते हैं।

एक और उपयोगी जानकारी है कि आप वेब पृष्ठ के साथ कैसे काम कर सकते हैं। इसमें आप विभिन्न एचटीएमएल टैग के साथ कैसे काम कर सकते हैं, कैसे डेटा को XPath या CSS सिलेक्टर का उपयोग करके प्राप्त कर स