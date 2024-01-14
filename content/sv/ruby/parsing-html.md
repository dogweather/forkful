---
title:                "Ruby: Analysera html"
simple_title:         "Analysera html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att parsa HTML är en väsentlig del av webbutveckling. Genom att extrahera specifika data från en HTML-sida kan man automatisera processer, bygga webbspindlar och mycket mer. Det är en viktig färdighet inom Ruby-programmering.

## Så här gör du

För att parsa HTML i Ruby behöver vi använda oss av ett tillägg som heter Nokogiri. Detta tillägg gör att vi kan identifiera och läsa HTML-element och dess innehåll enkelt. Här är ett enkelt exempel:

```ruby
require 'nokogiri'
require 'open-uri'

# Ange URL:en för HTML-sidan du vill parsa
url = "https://www.example.com"

# Läs in HTML-koden från URL:en
html = open(url)

# Använd Nokogiri för att parsa HTML-koden
doc = Nokogiri::HTML(html)

# Visa hela HTML-koden
puts doc

# Visa titeln på sidan
puts doc.css("title").text

# Visa länkarna på sidan
doc.css("a").each do |link|
    puts link["href"]
end
```

Genom att använda Nokogiri och Ruby-kod ovan kan vi enkelt parsa HTML från en given URL och extrahera specifik data. Output från exemplet ovan kan se ut som följer:

```
<!DOCTYPE html>
<html>
  <head>
    <title>Exempel</title>
  </head>
  <body>
    <h1>Detta är en rubrik</h1>
    <p>Detta är en paragraf</p>
    <a href="https://www.example.com/about">Om oss</a>
    <a href="https://www.example.com/contact">Kontakta oss</a>
  </body>
</html>

Exempel
https://www.example.com/about
https://www.example.com/contact
```

## Djupdykning

Att parsa HTML kan vara en komplex process, men genom att använda Nokogiri och andra tillägg som CSS Selector kan vi enkelt identifiera specifika element och dess egenskaper. Det finns många guider och dokumentationer online som kan hjälpa dig att bli mer bekant med alla möjligheter som finns när man parsa HTML med Ruby.

## Se även

- [Nokogiri dokumentation](https://nokogiri.org/)
- [CSS Selector dokumentation](https://www.w3schools.com/cssref/css_selectors.asp)
- [HTML parsing tutorial](https://scrapinghub.com/guides/parsing-html-with-ruby-and-nokogiri)