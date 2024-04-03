---
date: 2024-01-20 17:44:53.028836-07:00
description: "G\xF6r s\xE5 h\xE4r: Ruby erbjuder m\xE5nga s\xE4tt att ladda ner webbsidor,\
  \ men h\xE4r anv\xE4nder vi `net/http` biblioteket som \xE4r inbyggt och rakt p\xE5\
  \ sak."
lastmod: '2024-03-13T22:44:38.429701-06:00'
model: gpt-4-1106-preview
summary: "Ruby erbjuder m\xE5nga s\xE4tt att ladda ner webbsidor, men h\xE4r anv\xE4\
  nder vi `net/http` biblioteket som \xE4r inbyggt och rakt p\xE5 sak."
title: "H\xE4mta en webbsida"
weight: 42
---

## Gör så här:
Ruby erbjuder många sätt att ladda ner webbsidor, men här använder vi `net/http` biblioteket som är inbyggt och rakt på sak.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Förväntat resultat:

```Ruby
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</head>
...
</html>
```

Om du behöver hantera cookies eller andra avancerade funktioner, använd en gem som `httparty` eller `rest-client`.

## Djupdykning:
I de tidiga dagarna av webben, var det vanligare att använda kommandoradsverktyg som `wget` eller `curl` för att ladda ner webbsidor. Med Ruby kan du dock göra detta inifrån programkoden, vilket ger dig mer flexibilitet. 

Att välja rätt verktyg är nyckeln här. Medan `net/http` är bra för grundläggande användning, kan gems som `Mechanize` eller `Nokogiri` (för att parse:a HTML) vara bättre för komplex scraping. 

När du laddar ner en webbsida, tänk på webbservicens policyer och rättsliga bestämmelser som GDPR. Överanvänd inte deras resurser och respektera `robots.txt` filen.

## Se även:
- Ruby Docs för Net::HTTP: [https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- HTTParty GitHub: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- Mechanize gem: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- Nokogiri gem för att parse:a HTML: [https://nokogiri.org/](https://nokogiri.org/)
