---
title:                "Pobieranie strony internetowej"
aliases: - /pl/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:36.361540-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
## Co i dlaczego?

Pobieranie strony internetowej to proces ściągania jej zawartości do analizy lub przetwarzania. Programiści robią to, by np. zbierać dane (web scraping), monitorować zmiany czy testować aplikacje.

## How to:
## Jak to zrobić:

Przykład w Ruby wykorzystuje gem `net/http` do pobrania strony:

```Ruby
require 'net/http'
require 'uri'

def download_webpage(url)
  uri = URI(url)
  response = Net::HTTP.get(uri)
  puts response
end

download_webpage('http://example.com')
```

Sample output (wyjście próbkowe):

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
...
</body>
</html>
```

## Deep Dive
## W głąb tematu

W Ruby, klasy z modułu `net/http` są używane od lat do interakcji z HTTP. Alternatywą jest `open-uri` dla prostych zastosowań lub gem `httparty` dla większej kontroli nad żądaniami. Głębsze szczegóły implementacji `net/http` obejmują obsługę różnych metod HTTP, szyfrowanie SSL/TLS i możliwość pracy z proxy.

## See Also
## Zobacz również

- Ruby `Net::HTTP` dokumentacja: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- `httparty` gem: https://github.com/jnunemaker/httparty
- Przewodnik po web scraping w Ruby: https://www.rubyguides.com/2012/01/web-scraping-tutorial-ruby/
