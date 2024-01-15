---
title:                "Ladda ner en webbsida"
html_title:           "Ruby: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att hämta en webbsida är en vanlig uppgift för utvecklare, oavsett om de vill använda data från sidan eller bara utforska dess innehåll. Ruby erbjuder ett enkelt och kraftfullt sätt att hämta en webbsida med bara några få rader kod.

## Så här gör du

```ruby
require 'open-uri'

# Hämta en webbsida
page = open('https://www.example.com')

# Läs in innehållet som en sträng
content = page.read

# Skriv ut strängen
puts content
```

Koden ovan visar hur man använder Ruby's inbyggda `open-uri` bibliotek för att hämta en webbsida och läsa in dess innehåll. Det är viktigt att använda `require 'open-uri'` för att få tillgång till bibliotekets funktioner.

Om man vill spara innehållet på en webbsida till en fil istället för att bara skriva ut det kan man använda följande kod:

```ruby
# Öppna filen för skrivning
file = open('min_fil.txt', 'w')

# Hämta och skriv innehållet från webbsidan till filen
file.write(page.read)

# Stäng filen
file.close
```

Nu kommer innehållet på webbsidan att sparas i en textfil med namnet "min_fil.txt". Om man vill lägga till mer kod efter `file.write(page.read)`, till exempel att manipulera innehållet på något sätt, måste man stänga filen och öppna den igen med `file = open('min_fil.txt', 'a')` innan man kan fortsätta skriva till den.

## Fördjupning

Förutom de grundläggande kodexemplen ovan finns det många fler möjligheter med webbhämtningar i Ruby. Till exempel kan man använda olika metoder för HTTP-anrop, som `open-uri` som vi redan har sett, eller `net/http` biblioteket för mer avancerade funktioner.

Man kan också använda inbyggda parser-funktioner, som `Nokogiri`, för att bearbeta och extrahera data från webbsidorna man hämtar. Detta kan vara särskilt användbart för webbsidor med komplexa strukturer eller HTML-kod.

För mer information om alla möjligheter med att hämta webbsidor i Ruby, kolla in dokumentationen för `open-uri` och `net/http`, samt olika tillgängliga parser-bibliotek.

## Se även

- [OpenURI dokumentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)
- [Net/HTTP dokumentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Nokogiri dokumentation](https://nokogiri.org/)