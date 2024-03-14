---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:45.846938-07:00
description: "Een datum ontleden betekent een tekenreeks vertalen naar iets wat de\
  \ computer begrijpt\u2014zoals een echt datumobject. Programmeurs doen dit omdat\
  \ we vaak\u2026"
lastmod: '2024-03-13T22:44:51.348160-06:00'
model: gpt-4-0125-preview
summary: "Een datum ontleden betekent een tekenreeks vertalen naar iets wat de computer\
  \ begrijpt\u2014zoals een echt datumobject. Programmeurs doen dit omdat we vaak\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum ontleden betekent een tekenreeks vertalen naar iets wat de computer begrijpt—zoals een echt datumobject. Programmeurs doen dit omdat we vaak data als tekenreeksen oppikken van plaatsen zoals formulieren, bestanden of het web, en we moeten ermee werken op een gestructureerde, betrouwbare manier.

## Hoe te:

Ruby staat voor je klaar met zijn `Date`-bibliotheek, die het omzetten van tekenreeksen naar data zo makkelijk maakt als appeltaart. Vergeet alleen niet `require 'date'` te doen voordat je begint.

```ruby
require 'date'

# Parseer een datum (ISO-formaat) uit een tekenreeks
datum_tekenreeks = "2023-04-01"
geparseerde_datum = Date.parse(datum_tekenreeks)
puts geparseerde_datum
# => 2023-04-01

# Wat als ons formaat ontspoort? Probeer dit.
begin
  aangepaste_datum = Date.strptime('03/31/2023', '%m/%d/%Y')
  puts aangepaste_datum
rescue ArgumentError
  puts "Dat is geen geldig datumformaat, maat."
end
# => 2023-03-31
```

## Diepe Duik

Vroeger was Ruby minder vergevingsgezind met datumformaten. Coders moesten handmatig worstelen met tekenreeksen om data te onttrekken. Nu speurt `Date.parse` automatisch de meest voorkomende datumformaten op, en als het in de war raakt, laat `Date.strptime` je het exacte formaat specificeren om misinterpretaties te vermijden.

Alternatief-wise, als je te maken hebt met meer complexe datum-tijdgegevens, is `DateTime` misschien je van pas, vooral voor het ontleden van tijden ook. Bovendien, voor degenen die buiten de standaardbibliotheek van Ruby spelen, is er de `Chronic`-gem door de slimme mensen bij GitHub, die een hoop natuurlijke taal-datumuitdrukkingen begrijpt.

Onder al deze eenvoud wordt het ontleden van Ruby daadwerkelijk aangedreven door datumformaat sjablonen die verschillende delen van de tekenreeks met datum matchen met de overeenkomstige datumelementen (jaar, maand, dag, enz.). Dus wanneer je tekenreeks niet overeenkomt met het verwachte patroon, moet je Ruby een seintje geven met `strptime` en de juiste formaatdirectieven.

## Zie Ook

- Voor tijdzonefans kan de documentatie van `ActiveSupport::TimeWithZone` in Rails interessant zijn: [https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
- Chronic-gem voor het ontleden van datums in natuurlijke taal: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
