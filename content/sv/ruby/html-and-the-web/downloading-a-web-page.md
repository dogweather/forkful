---
date: 2024-01-20 17:44:53.028836-07:00
description: "Att ladda ner en webbsida betyder att h\xE4mta dess HTML-inneh\xE5ll.\
  \ Programmerare g\xF6r det f\xF6r att analysera sidans struktur, skrapa data eller\
  \ testa sidans\u2026"
lastmod: '2024-03-11T00:14:11.836704-06:00'
model: gpt-4-1106-preview
summary: "Att ladda ner en webbsida betyder att h\xE4mta dess HTML-inneh\xE5ll. Programmerare\
  \ g\xF6r det f\xF6r att analysera sidans struktur, skrapa data eller testa sidans\u2026"
title: "H\xE4mta en webbsida"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida betyder att hämta dess HTML-innehåll. Programmerare gör det för att analysera sidans struktur, skrapa data eller testa sidans uppladdningstid.

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
