---
title:                "Elixir: एक नया प्रोजेक्ट शुरू करना"
simple_title:         "एक नया प्रोजेक्ट शुरू करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

नए प्रोजेक्ट की शुरुआत क्यों करें?

एक नए प्रोजेक्ट की शुरुआत करना एक बहुत ही महत्वपूर्ण फैसला हो सकता है। यह आपको नए सीखने का मौका देता है और आपको आपके कौशल का पूरा उपयोग करने की अनुमति देता है।

कैसे करें?

```Elixir
defmodule Project do
  IO.puts("नए प्रोजेक्ट की शुरुआत कैसे करें?") 
end
```

इसके बाद, आपको एक नया फ़ोल्डर बनाना होगा जिसमें आपने अपना प्रोजेक्ट स्ट्रक्चर सेटअप किया है। उसके बाद आप अपने प्रोजेक्ट को कड़ीबद्ध कर सकते हैं। उदाहरण के लिए, आप गूगल अनुवाद एपीआई का उपयोग कर सकते हैं जो एक अच्छा समझौता है।

```Elixir
defmodule Translator do
  require HTTPoison
  require Poison

  def translate(text, lang) do
    url = "https://translation.googleapis.com/language/translate/v2"
    body = %{
      q: text,
      target: lang,
      key: "your_api_key"
    }
    response = HTTPoison.post(url, body, [])

    case response do
      {:ok, %{status_code: 200, body: body}} ->
        translation = Poison.decode!(body)["data"]["translations"][0]["translatedText"]
        IO.puts("तर्जन: #{translation}")
      _ ->
        IO.puts("तर्जन असफल।")
    end
  end
end

#उदाहरण के लिए: अंग्रेजी से हिंदी में टेक्स्ट का तर्जन करें
Translator.translate("Hello World!", "hi")
```

डीप डाइव:

एक नया प्रोजेक्ट शुरू करने के लिए कुछ महत्वपूर्ण तरीके हैं। आपको एक अच्छा स्ट्रक्चर सेटअप करने की आवश्यकता होगी ताकि आपका कोड साफ हो और उत्तरदायी हो। साथ ही, आपको एक सुचारू नाम देना चाहिए जो कि फुचर में आपके प्रोजेक्ट को अलग बनाए रखेगा। इसके अलावा, आपको अपने कोड को बनाई रहने के