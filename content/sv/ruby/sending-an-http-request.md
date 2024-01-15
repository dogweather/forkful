---
title:                "Skicka en http-begäran"
html_title:           "Ruby: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför?

För att hämta data från en webbsida eller API, krävs det att skicka en HTTP-anrop. Detta gör att det blir möjligt att få tillgång till information från olika webbplatser och använda den i dina egna applikationer.

## Hur man gör

För att skicka en HTTP-anrop i Ruby, kan du använda dig av Ruby's inbyggda Net::HTTP modul. Detta är ett exempel på hur du kan göra ett GET-anrop till en webbplats:

```Ruby
require 'net/http'

url = URI.parse('https://www.example.com')
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true # Om webbplatsen använder HTTPS
response = http.get(url)
puts response.body # Skriver ut den hämtade datan
```

```Ruby
require 'net/http'

url = URI.parse('https://www.example.com/api')
params = { key: 'value' } # Data som jag vill skicka
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true # Om webbplatsen använder HTTPS
response = http.post(url, params)
puts response.body # Skriver ut svaret från servern
```

## Djupdykning

För att skicka ett HTTP-anrop behöver du förstå de olika delarna av ett HTTP-anrop. Detta inkluderar metoden (GET, POST, PUT etc.), URL-en, och eventuell data som ska skickas med anropet.

Du kan också sätta HTTP-headers i ditt anrop för att skicka ytterligare information. Till exempel, om du ska skicka autentiseringsuppgifter till en API, kan du sätta en "Authorization" header i ditt anrop.

## Se även

- [Net::HTTP dokumentation](https://ruby-doc.org/stdlib-2.7.4/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Request Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [HTTP Headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers)