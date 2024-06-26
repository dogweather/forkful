---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:23.516785-07:00
description: 'Hoe: De standaardbibliotheek van Ruby biedt `Tempfile` voor het maken
  van tijdelijke bestanden. Laten we er direct induiken.'
lastmod: '2024-03-13T22:44:51.379678-06:00'
model: gpt-4-0125-preview
summary: De standaardbibliotheek van Ruby biedt `Tempfile` voor het maken van tijdelijke
  bestanden.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe:
De standaardbibliotheek van Ruby biedt `Tempfile` voor het maken van tijdelijke bestanden. Laten we er direct induiken:

```Ruby
require 'tempfile'

Tempfile.create('my_temp') do |tempfile|
  tempfile.write('Tijdelijke inhoud')
  puts "Tijdelijk bestand bevindt zich op: #{tempfile.path}"
end
# Na het blok wordt het bestand automatisch verwijderd.
```

Zodra je dit uitvoert, zie je:

```
Tijdelijk bestand bevindt zich op: /tmp/my_temp20180418-56789-1234567
```

Dit bestand blijft niet langer dan nodig. Zodra het blok eindigt, ruimt Ruby voor je op.

## Uitdieping
De `Tempfile`-klasse bestaat al sinds Ruby 1.8 en is in de loop van de tijd geoefend en verfijnd. Onder de motorkap gebruikt het het tijdelijke bestandspad van je systeem, aangeboden door het besturingssysteem.

Alternatieven? Zeker, je zou handmatig tijdelijke bestanden kunnen maken en bijhouden, maar waarom het wiel opnieuw uitvinden? `Tempfile` geeft je een willekeurige, unieke bestandsnaam, waardoor het risico op botsingen vermindert.

Voor degenen die meer controle verlangen, neemt de methode `Tempfile.new` parameters voor het aanpassen van de bestandsnaam en locatie. Maar onthoud, met grote macht komt grote verantwoordelijkheid - je zult deze bestanden handmatig moeten verwijderen.

Het echte voordeel van het gebruik van `Tempfile` ligt in zijn thread-safe en garbage-collected natuur. Het sluit het bestand af en zorgt ervoor dat gevoelige gegevens niet langer blijven dan zou moeten. Een tijdelijk bestand gedraagt zich veel als een standaardbestandsobject, dus je kunt het lezen, naar schrijven en anderszins manipuleren met behulp van typische bestandsbewerkingen.

## Zie Ook
- Ruby API Dock voor diepere Tempfile-gebruiksvoorbeelden: [API Dock Tempfile](https://apidock.com/ruby/Tempfile)
- Gids voor Ruby-bestands-I/O voor meer over het afhandelen van bestanden: [Bestands-I/O](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
