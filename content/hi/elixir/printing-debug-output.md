---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Elixir: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

डीबग आउटपुट प्रिंट करने का मुख्य कारण हर कोडर के लिए विशेष नहीं है, लेकिन यह एक महत्वपूर्ण टूल है जो वे कोड के साथ दोस्ताना कड़ी बात करता है।

## कैसे करें

एक सार्वजनिक फंक्शन को बनाएं जो आपको अपने एप्लिकेशन में डीबग का विस्तारीकरण करने में मदद करेगा:

```Elixir
defmodule Debug do
  def print_debug(output) do
    IO.puts "Debug: #{inspect output}"
  end
end
```

अपने एप्लिकेशन में डीबग आउटपुट प्रिंट करने के लिए, आप उपयोगकर्ता द्वारा प्रदान की गई पैरामीटर को फंक्शन `print_debug` के साथ बुलाएं:

```Elixir
Debug.print_debug("Sample output")
```

आपको निम्नलिखित कुछ ऐसे आउटपुट मिलेंगे:

```
Debug: "Sample output"
```

## गहराई में जाएं

डीबग आउटपुट प्रिंट करने का एक तीसरा और अधिक विस्तृत कारण यह है कि आप अपने कोड की गहराई को समझने में मदद कर सकते हैं। डीबग आउटपुट विन्यास और फिल्टरिंग के लिए फंक्शन `IO.inspect` का उपयोग करें:

```Elixir
user = %{name: "John", age: 30, email: "johndoe@example.com"}
IO.inspect user, only: [:name, :age]
```

आपको निम्नलिखित कुछ ऐसे आउटपुट मिलेंगे:

```
%{age: 30, name: "John"}
```

## देखें भी

- [Elixir डॉक्स](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir की आधिकारिक वेबसाइट](https://elixir-lang.org/)