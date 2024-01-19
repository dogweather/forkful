---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran innebär att begära data från en annan server genom internetprotokollet HTTP (Hypertext Transfer Protocol). Programmerare gör detta för att interagera med webbtjänster, uppnå datautbyte och möjliggöra flera andra funktioner i sina program.


## Hur man:

För att skicka HTTP-begäran i Ruby, använder vi ofta the `net/http` bibliotektet. Här är några exempel:

```Ruby
require 'net/http' 
require 'uri'

uri = URI.parse("http://example.com/") 
response = Net::HTTP.get_response(uri)

puts response.body
```

När du kör det här programmet, tar det kontakt med webbservraren på http://example.com/ och returnerar sedan svar från webbplatsen.

## Djup Dykning:

### Historisk Kontext
HTTP-protokollet lanserades 1991 och används för kommunikation på världsomspännande webben. Ruby själv lanserades 1995, och möjligheten att skicka HTTP-begäran byggdes in i Ruby-biblioteket tillsammans med andra nätverksfunktioner.

### Alternativ
I Ruby, finns det andra bibliotek som kan användas för att skicka HTTP-begäran, som 'faraday', 'httparty' och 'rest-client'. Med 'faraday' till exempel, kan du använda flera olika adapter för att skicka begäranden.

### Implementationsdetaljer
`net/http` biblioteket använder en instans av `Net::HTTP::Get` (eller `Post`, `Put`, etc.) för att skapa en HTTP-begäran. Den här begäran skickas sedan till servern med `Net::HTTP#request` metoden.

## Se Även

- HTTP-protokollet: www.w3.org/Protocols/
- 'net/http' dokumentation: www.ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html
- Alternativa bibliotek: 'faraday'(github.com/lostisland/faraday), 'httparty'(github.com/jnunemaker/httparty), 'rest-client'(github.com/rest-client/rest-client).