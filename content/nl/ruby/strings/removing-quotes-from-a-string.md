---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:35.455714-07:00
description: "Het verwijderen van aanhalingstekens uit een string betekent het wegpellen\
  \ van die dubbele of enkele aanhalingstekens die rondom tekstwaarden wikkelen.\u2026"
lastmod: '2024-03-13T22:44:51.322611-06:00'
model: gpt-4-0125-preview
summary: Het verwijderen van aanhalingstekens uit een string betekent het wegpellen
  van die dubbele of enkele aanhalingstekens die rondom tekstwaarden wikkelen.
title: Quotes verwijderen uit een string
weight: 9
---

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een string betekent het wegpellen van die dubbele of enkele aanhalingstekens die rondom tekstwaarden wikkelen. Programmeurs doen dit vaak om gebruikersinvoer op te schonen, om consistentie in gegevensverwerking te waarborgen, of om gegevens voor te bereiden voor systemen die in de war kunnen raken door die extra karakters.

## Hoe:
Ruby heeft enkele slimme trucjes om die lastige aanhalingstekens weg te knippen. Je kunt de methoden `gsub` of `delete` gebruiken om de klus te klaren. Hier is wat code om op te kauwen:

```ruby
# Met gsub dubbele en enkele aanhalingstekens verwijderen
quoted_string = "\"Zeg 'hallo' tegen mijn kleine vriend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Uitvoer: Zeg hallo tegen mijn kleine vriend!

# Als je weet dat je alleen met één type aanhalingsteken te maken hebt
single_quoted_string = "'Blijf een tijdje en luister!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Uitvoer: Blijf een tijdje en luister!
```

## Diepgaande Duik
De geschiedenis van aanhalingstekens gaat terug tot de vroegste dagen van programmeren, waar ze vaak dienden als stringdelimiters. Vandaag de dag, net als toen, zou je jezelf kunnen vinden in de situatie dat je deze aanhalingstekens moet verwijderen wanneer ze niet nodig zijn of wanneer ze kunnen interfereren met gegevensopslag en -manipulatie.

We hebben het gehad over `gsub` en `delete`, maar er zijn ook andere methoden, zoals `tr` of `tr_s`, die je een beetje meer controle geven of sommige verschillende use cases kunnen behandelen:

```ruby
# tr kan ook aanhalingstekens verwijderen
double_quoted_string = "\"Doen of niet doen, er is geen poging.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Uitvoer: Doen of niet doen, er is geen poging.
```

Onthoud dat elke methode zijn gebruiksscenario's heeft. `gsub` is krachtiger wanneer je te maken hebt met complexe patronen of meerdere vervangingen. `delete` en `tr` werken prachtig voor eenvoudige, rechtlijnige karakterverwijderingen.

## Zie Ook
Voor aanvullend leesmateriaal, en om deze methoden in actie te zien binnen grotere codebases, bekijk:
- De Ruby-documentatie voor [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), en [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas heeft een geweldige [String-oefenset](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), die werken met aanhalingstekens omvat.
- Stack Overflow discussies over [stringmanipulatie](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) bieden echte problemen en oplossingen van mede-Rubyisten.
