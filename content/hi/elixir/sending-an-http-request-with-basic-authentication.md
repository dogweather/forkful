---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP request भेजने का तरीका, basic authentication के साथ, वेब सर्वर्स से डेटा संवाधन प्राप्त करने का एक साधारण तरीका है। प्रोग्रामर्स इसे इसलिए इस्तेमाल करते हैं क्योंकि यह एक सुरक्षित तरीका है डेटा को अन्य सिस्टमों से प्राप्त करने का।

## कैसे करें:

HTTPoison, जो Elixir के साथ काम करने के लिए एक HTTP client library है, आसानी से इसे इस्तेमाल कर सकते हैं। 

```elixir
HTTPoison.Basic.start
{:ok, response} = HTTPoison.get "http://example.com", [], basic_auth: {"user", "pass"}
IO.inspect response.status_code
```

यह कोड एक get request भेजता है "http://example.com" पर, और यह अप्रत्याशित प्रतिक्रिया के status code को मुद्रित करता है।

## विस्तार में 

यद्यपि Basic Authentication आसान और सीधे है, यह सबसे सुरक्षित तरीका नहीं है। असलियत में, यह बस Base64 encoding का उपयोग करता है, जो बहुत आसानी से डिकोड किया जा सकता है। इसका प्रयोग तब किया जाना चाहिए जब आपके पास बेहतर विकल्पों की कमी हो। 

जब बेहतर सुरक्षा की आवश्यकता हो, तो ओAuth और JWT (JSON Web Tokens) जैसे अधिकतर सुरक्षित और लोकप्रिय विकल्प उपलब्ध हैं।

## देखे भी 

1. [HTTPoison documentation](https://hexdocs.pm/httpoison/readme.html)
2. [Elixir School on HTTPoison](https://elixirschool.com/en/lessons/libraries/httpoison/)
3. [Basic Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)