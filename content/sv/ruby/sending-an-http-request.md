---
title:                "Skicka en http-förfrågan."
html_title:           "Ruby: Skicka en http-förfrågan."
simple_title:         "Skicka en http-förfrågan."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran är när en programör använder ett programmeringsspråk som Ruby för att skicka ett begärande med specifik information till en annan server. Detta kan inkludera att hämta data från en annan webbsida eller att skicka information till en annan applikation.

Varför skickar programörer HTTP-begäran? Detta kan bero på olika skäl, till exempel att hämta information och använda den i sin egen applikation, eller att integrera med andra applikationer för ett smidigare användarupplevelse.

## Hur gör man:

För att skicka en HTTP-begäran med Ruby, kan du använda "Net::HTTP" modulen som finns tillgänglig i Ruby standardbiblioteket. Låt oss säga att vi vill hämta innehållet på en specifik webbsida, till exempel Wikipedia:s startsida:

```
require 'net/http'

uri = URI('https://sv.wikipedia.org/wiki/Wikipedia:Huvudsida')
response = Net::HTTP.get_response(uri)

puts response.body
```

Detta kommer att skriva ut hela innehållet på Wikipedia:s huvudsida i vår terminal, eftersom vi använder "puts" för att skriva ut innehållet i "response.body" variabeln.

## Djupdykning:

I dagens internetålder är det vanligt för applikationer att behöva integrera med andra applikationer, och därför är skicka HTTP-begäran en viktig del av en programmerarens verktygslåda. Innan "Net::HTTP" modulen fanns tillgänglig i standardbiblioteket, behövde man använda tredjepartsbibliotek som "HTTParty" eller "RestClient" för att skicka HTTP-begäran.

Om du är intresserad av att lära dig mer om HTTP, kan du läsa på om dess historiska kontext och olika versioner för att få en djupare förståelse för protokollet. Det finns också alternativa sätt att skicka HTTP-begäran, som till exempel att använda cURL kommandoraden i terminalen.

## Se även:

- [Ruby standardbiblioteket: Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty](https://github.com/jnunemaker/httparty)
- [RestClient](https://github.com/rest-client/rest-client)
- [En djupdykning i HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)