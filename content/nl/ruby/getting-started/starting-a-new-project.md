---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:49.479567-07:00
description: "Een nieuw project starten is als het planten van een zaadje in je digitale\
  \ tuin - je begint een frisse bundel idee\xEBn, en zet deze om in code die iets\u2026"
lastmod: '2024-03-11T00:14:25.202490-06:00'
model: gpt-4-0125-preview
summary: "Een nieuw project starten is als het planten van een zaadje in je digitale\
  \ tuin - je begint een frisse bundel idee\xEBn, en zet deze om in code die iets\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten is als het planten van een zaadje in je digitale tuin - je begint een frisse bundel ideeën, en zet deze om in code die iets nuttigs doet. Programmeurs starten nieuwe projecten om problemen op te lossen, concepten te verkennen, of gewoon voor het pure plezier van het creëren van iets nieuws.

## Hoe:

Dus, je bent klaar om die hersengolven om te zetten in een Ruby project? Laten we beginnen. Start met de basis.

```Ruby
# Installeer Ruby, als je dat nog niet hebt gedaan.
# Controleer je Ruby-versie om er zeker van te zijn dat je up-to-date bent:
$ ruby -v

# De uitvoer zou de huidige versie van Ruby moeten zijn:
# ruby 3.x.x

# Volgende, laten we een directory maken voor je project:
$ mkdir my_new_project
$ cd my_new_project

# Initieer nu een nieuwe Git-repository als je versiebeheer wilt (sterk aanbevolen):
$ git init

# Maak dan een entry-bestand, laten we het 'app.rb' noemen:
$ touch app.rb

# Begin met coderen! Schrijf een eenvoudige output om te zorgen dat het werkt:
puts "Hallo Nieuw Project!"

# Voer je bestand uit:
$ ruby app.rb

# De uitvoer zou moeten zijn:
# Hallo Nieuw Project!
```

## Diepere Duik

In het verleden was het starten van een nieuw Ruby-project een beetje rauw - gewoon jij, een teksteditor, en een hoop `.rb` bestanden. Naarmate de taal gegroeid is, zijn er hulpmiddelen verschenen om dit proces te stroomlijnen.

Bijvoorbeeld, Bundler beheert je gems - Ruby-bibliotheken - zodat je eenvoudig afhankelijkheden kunt bijhouden en installeren. Voer gewoon `bundle init` uit nadat je je projectdirectory hebt opgezet om een `Gemfile` te creëren, waarin je je gems opsomt.

Dan hebben we Ruby Version Manager (RVM) en Ruby Environment (rbenv), die helpen om per project tussen Ruby-versies te schakelen. Best handig als je oudere code jongleert.

En hoe zit het met frameworks? Ruby on Rails is de grote naam voor webapps. Maar als je lichtgewicht gaat (zoals voor diensten of API's), bekijk dan Sinatra of Roda.

## Zie Ook

- Ruby's officiële site voor updates en documentatie: [https://www.ruby-lang.org](https://www.ruby-lang.org)
- Bundler, om je Ruby gems te beheren: [https://bundler.io](https://bundler.io)
- RVM, een Ruby Version Manager: [https://rvm.io](https://rvm.io)
- rbenv, om een Ruby-versie voor je project te kiezen: [https://github.com/rbenv/rbenv](https://github.com/rbenv/rbenv)
- Sinatra, een lichtgewicht webframework: [http://sinatrarb.com](http://sinatrarb.com)
- Voor code delen en samenwerking is GitHub je beste keuze: [https://github.com](https://github.com)
