---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing av HTML med Ruby: En Praktisk Guide

## Hva & Hvorfor?
Parsing av HTML handler om å analysere og oversette html koden til et format som er mindre komplekst, og derfor mer håndterbart. Programmere gjør det for å manipulere, hente eller skrape data fra web-sider.

## Hvordan gjøre: 
Vi skal bruke en Ruby gem `Nokogiri` for vår oppgave. La oss prøve noen koder.

Installer Nokogiri først med: `gem install nokogiri`.

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(URI.open("https://www.example.com"))

doc.css('h1').each do |title|
  puts title.text
end
```

Dette vil skrape og vise all tekst innkapslet i h1 tags fra "www.example.com”.

## Dyp Dykk
1) HTML parsing har en lengre historisk kontekst—fra webskraping til dataanalyse—og har endret hvordan vi interagerer med weben. 
2) Alternativene til Nokogiri inkluderer hpricot og mechanize. Men Nokogiri er ofte foretrukket for sin raskhet og pålitelighet.
3) Parsing av HTML med Ruby og Nokogiri involverer flere trinn: henting av HTML-dokumenter, traversering av DOM og henting av den nødvendige data.

## Se Også
1) [Nokogiri offisiell dokumentasjon](https://nokogiri.org/)
2) [En detaljert guide til web-skraping med Ruby og Nokogiri](https://www.rubyguides.com/ruby-tutorial/html-parsing/)
3) [Cookbook for å forstå HTML Parse-trær](https://tenderlovemaking.com/2009/12/04/html-tree-parsing-and-searching-with-nokogiri.html)