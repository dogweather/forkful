---
title:                "Ruby: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside kan være nyttig for å hente informasjon fra en nettside, som for eksempel data om en produktside, prisene på et nettsted eller lignende.

## Hvordan gjøre det

Det er enkelt å laste ned en nettside ved hjelp av Ruby kode. Først må du installere et bibliotek kalt "Net/HTTP" ved bruk av RubyGems. Deretter kan du bruke følgende kode for å laste ned en nettside:

```Ruby
require 'net/http'

url = URI('urlen til nettsiden du vil laste ned')

res = Net::HTTP.get_response(url)
puts res.body
```

Koden over vil hente nettsiden og skrive ut innholdet som respons. Du kan også gjøre et GET request direkte til nettsiden ved hjelp av følgende kode:

```Ruby
require 'net/http'

url = URI('urlen til nettsiden du vil laste ned')

res = Net::HTTP.get(url)
puts res
```

I tillegg kan du bruke "open-uri" biblioteket for å laste ned en nettside på en mer enkel måte:

```Ruby
require 'open-uri'

url = 'urlen til nettsiden du vil laste ned'
puts open(url).read
```

## Dypdykk

Når du laster ned en nettside, vil du også laste ned all HTML-koden og eventuelle bilder og andre ressurser som er på nettsiden. Dette innebærer at du kan analysere og bearbeide koden for å få ut spesifikk informasjon, som for eksempel ved å bruke søkemotorer eller scrapping verktøy. Du kan også bruke ekstra parametere i GET requestet, som for eksempel å legge til en brukeragent eller begrense resultatene til bare å vise teksten uten HTML-taggene.

## Se også

- [Net/HTTP biblioteket for Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Open-uri biblioteket for Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)
- [Scrapping med Ruby](https://realpython.com/beautiful-soup-web-scraper-python/)