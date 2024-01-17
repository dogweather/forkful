---
title:                "Å laste ned en nettside"
html_title:           "Ruby: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Når vi snakker om å laste ned en nettside i programmering, betyr det rett og slett å hente nettstedets innhold til vår egen datamaskin. Dette kan være nyttig for å analysere og behandle data eller for å automatisere handlinger som å lagre informasjon eller skrape informasjon fra nettsider. Programmere laster ned nettsider for å effektivt arbeide og manipulere data.

# Slik gjør du:
```ruby
require 'net/http'
url = "www.example.com"
page = Net::HTTP.get(URI.parse(url))
puts page
```

Denne koden bruker Ruby sitt innebygde bibliotek for å laste ned nettsiden ved hjelp av netthåndteringsmodulen. Den angitte URL-en blir åpnet og nettstedets innhold blir hentet og lagt inn i variabelen "side". Deretter blir innholdet av nettstedet skrevet ut på skjermen.

# Dykk dypere:
I eldre versjoner av Ruby, måtte man bruke et eksternt bibliotek for å laste ned nettsider. Dette var ofte komplisert og krevde mye kode. Nå er det mye enklere med det innebygde biblioteket net/http. Alternativt kan man også bruke biblioteket open-uri, men dette kan være mindre effektivt.

Det kan være lurt å inkludere feilhåndtering i koden for å håndtere situasjoner der nettsiden ikke kan lastes ned eller når det oppstår andre problemer. Det er også viktig å være oppmerksom på opphavsrett og ikke laste ned store mengder data uten tillatelse.

# Se også:
- https://github.com/ruby/net-http
- https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html
- https://www.rubyguides.com/2018/01/ruby-http-request/