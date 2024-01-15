---
title:                "एक http अनुरोध भेजना"
html_title:           "Elixir: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एक HTTP अनुरोध भेजने में किसी को कितना रुचि हो सकता है, इसे समझने के लिए सूचना उपलब्ध कराने से संबंधित हो सकता है।

## कैसे करें

```elixir
defmodule HTTPRequest do
  def send(method, url) do
    case method do
      "GET" -> IO.puts("Request sent to #{url} using the GET method.")
      "POST" -> IO.puts("Request sent to #{url} using the POST method.")
      "PUT" -> IO.puts("Request sent to #{url} using the PUT method.")
      "DELETE" -> IO.puts("Request sent to #{url} using the DELETE method.")
      _ -> IO.puts("Invalid method.")
    end
  end
end

method = "GET"
url = "https://example.com"
HTTPRequest.send(method, url)
```

इस कोड ब्लॉक देखकर आप एक एचटीटीपी अनुरोध भेजने के लिए कैसे कोड कर सकते हैं और कैसे उसकी उत्पादन की जांच कर सकते हैं।

## गहराई में जाएँ

HTTP अनुरोध भेजने के लिए, हम ईक्सर का अनुकरण कर सकते हैं, जो कि असंबद्ध और एसिन्क्रोनस कार्रवाई के लिए भी जाना जाता है। HTTPoison और Mint जैसे कई पैकेज उपलब्ध हैं जो HTTP अनुरोधों को अधिक सहज बनाने के लिए आसान माध्यम प्रदान करते हैं।

## देखें सहायक

[HTTPoison](https://hex.pm/packages/httpoison)

[Mint](https://hex.pm/packages/mint)

[An Introduction to HTTP Clients in Elixir](https://www.syndicode.com/blog/2019/02/22/an-introduction-to-http-clients-in-elixir/)