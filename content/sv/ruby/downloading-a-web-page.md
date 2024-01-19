---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att hämta en webbsida innebär att man hämtar data från en webbsida till ens egen dator. Detta är viktigt för programmerare eftersom det tillåter dem att manipulera, analysera eller modifiera den data som är tillgänglig på sidan.

## Hur man gör:
För att hämta en webbsida i Ruby, kan du använda 'Net::HTTP'-biblioteket vilket ingår i Ruby's standardbibliotek. Här är ett enkelt exempel:

```Ruby
require 'net/http'
  
url = URI('http://example.com')
response = Net::HTTP.get_response(url)
puts response.body
```

När du kör den här koden, kommer du att se webbsidans HTML-struktur i din terminal.

## Djupdykning
Förmågan att hämta en webbsida i ett programmeringsspråk går tillbaka till de tidiga dagarna av Internet, när webbläsare var praktiskt taget icke-existerande och all interaction med webben skedde via kommandoraden.

Modern Ruby erbjuder flera alternativ för att hämta en webbsida. Förutom 'Net::HTTP', finns det andra bibliotek som 'Open-URI' och 'Faraday' som erbjuder mer avancerade funktioner.

När det kommer till implementationen går Net::HTTP först igenom DNS-uppslaget, initierar en TCP-anslutning till servern och skickar sedan en HTTP-begäran. Svaret returneras som ett 'Net::HTTPResponse'-objekt som du kan använda för att manipulera data vid behov.

## Se också
För mer information om att hämta en webbsida i Ruby, se följande resurser:

1. [Net::HTTP Dokumentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
2. [Open-URI Dokumentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html)
3. [Faraday införande](https://lostisland.github.io/faraday/)
4. [Web Scraping med Ruby](https://coderwall.com/p/ek0pqg/web-scraping-in-ruby)