---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग, एक HTML डॉक्यूमेंट को विश्लेषित करने और उसके विभिन्न भागों को पहचानने की प्रक्रिया है। प्रोग्रामर्स इसे डाटा माइनिंग, वेब स्क्रेपिंग, और मशीन की अनुकूलता के लिए करते हैं।

## कैसे करें:

पार्स HTML डॉक्यूमेंट्स तैयार करने के लिए Elixir में Floki लाइब्रेरी का उपयोग कर सकते हैं।

```Elixir
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```

अगले, निम्नवत प्रोग्राम का उपयोग करके HTML एलिमेंट्स को fetch और filter करें।

```Elixir
{:ok, document} = Floki.parse_document("<html><title>Hello!</title></html>")
title = document |> Floki.find("title") |> Floki.raw_html
IO.puts(title)
```

Output कुछ इस प्रकार दिखाई देता है:

```Elixir
Hello!
```

## गहरी जांच:

HTML पार्सिंग का इतिहास वेब के साथ-साथ शुरू हुआ था; गौरतलब है, HTML पार्सिंग मैन्युअल और क्लंकी था। Elixir, जैसी मॉडर्न लैंग्वेजेस ने इसे काफी सरल बना दिया है। विकल्प जैसे जावास्क्रिप्ट (जेसीओपियान) और पायथन (Beautiful Soup) पेश करते हैं, लेकिन Elixir की वजह से मल्टी-थ्रेडिंग और हाई परफॉर्मेंस का वादा HTML पार्सिंग को उनसे बेहतर बनाता है। 

## देखे भी:

- Floki documentation: https://hexdocs.pm/floki/readme.html
- Other Elixir HTML parsing options: https://elixirforum.com/t/what-is-the-way-to-parse-html-in-elixir/2182/2
- Introduction to web scraping with Elixir: https://dev.to/kieraneglin/introduction-to-web-scraping-with-elixir-2f4b