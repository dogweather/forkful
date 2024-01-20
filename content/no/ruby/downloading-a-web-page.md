---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Nedlasting av en webside er prosessen med å kopiere og bevare innholdet på en side, fra serveren til din lokale maskin. Dette vil ofte bli gjort for å analysere eller behandle dataen på siden, for eksempel snarveier, priser, tekst og mer.

## Hvordan?

Her viser vi hvordan benytte biblioteket 'open-uri' for Ruby.

```Ruby
require 'open-uri'

URL = 'https://www.example.com'

begin
  web_content = URI.open(URL).read
  puts web_content
rescue => e
  puts "Feil oppstod: #{e}"
end
```

Når du kjører denne koden, vil den laste ned og skrive ut innholdet på siden www.example.com. Hvis den finner på feil i processen, vil den skrive ut en feilmelding.

## Dypdykk

Historisk har programmerere lastet ned websider ved hjelp av HTTP forespørsler. Selv om dette fortsatt er en gyldig metode, gjør biblioteker som 'open-uri' i Ruby det lettere ved å håndtere detaljene for oss.

Alternative metoder inkluderer bruk av andre biblioteker som 'Net::HTTP' eller 'HTTParty', som gir flere alternativer for tilpasning. 

Skulle du ha behov for å laste ned store deler av en nettside, bør du benytte 'robots.txt' for å respektere nettsidens regler. Og husk å alltid respektere opphavsretten til innholdet du laster ned.

## Se også:

For mer informasjon, se følgende lenker:

- Dokumentasjon for 'open-uri': https://ruby-doc.org/stdlib-3.0.2/libdoc/open-uri/rdoc/OpenURI.html
- Blogginnlegg om bruk av 'Net::HTTP': https://www.rubyguides.com/2012/01/nethttp-tutorial/
- Dokumentasjon for 'HTTParty': https://www.rubydoc.info/github/jnunemaker/httparty
- Guide for 'robots.txt': https://developers.google.com/search/docs/advanced/robots/intro