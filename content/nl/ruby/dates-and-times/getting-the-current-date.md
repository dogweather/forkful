---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:23.974378-07:00
description: 'Hoe te: Ruby maakt het kinderlijk eenvoudig om de huidige datum te krijgen.
  Zo doe je dat.'
lastmod: '2024-03-13T22:44:51.349184-06:00'
model: gpt-4-0125-preview
summary: Ruby maakt het kinderlijk eenvoudig om de huidige datum te krijgen.
title: Het huidige datum ophalen
weight: 29
---

## Hoe te:
Ruby maakt het kinderlijk eenvoudig om de huidige datum te krijgen. Zo doe je dat:

```ruby
require 'date'

# Pak de huidige datum
current_date = Date.today
puts current_date
```

Het uitvoeren van dit fragment zal zoiets als dit afdrukken (afhankelijk van de dag dat je het uitvoert):

```
2023-04-07
```

Wil je ook de tijd? Hier is de code:

```ruby
require 'time'

# Pak de huidige datum en tijd
current_datetime = Time.now
puts current_datetime
```

En de output zal de tijdstempel bevatten:

```
2023-04-07 12:34:56 +0900
```

## Diepgaande Duik
Ooit hadden Rubyisten externe bibliotheken nodig om met datums en tijden om te gaan. Maar toen kwam de standaard Ruby bibliotheek met de `Date` en `Time` klassen, en was de noodzaak voor extra's grotendeels verleden tijd.

`Date` behandelt, nou ja, datums - dag, maand en jaar. Voor meer precisie combineert `DateTime` datum en tijd, maar als je alleen de tijd of meer gedetailleerde gegevens zoals seconden of tijdzones nodig hebt, dan biedt `Time` uitkomst.

Alternatieven voor Ruby's ingebouwde klassen zijn gems zoals 'timecop' voor het testen van tijdafhankelijke code, en 'chronic' voor het parsen van natuurlijke taal datums.

Onder de motorkap haalt `Date.today` de datum van je systeem op. Het houdt de zaken simpel, maar negeert tijdzones. `Time.now` gaat verder, rekening houdend met tijdzones met een standaard offset vanaf de Gecoördineerde Universele Tijd (UTC).

## Zie Ook
* Ruby's documentatie over de Time klasse: [https://ruby-doc.org/core-2.7.0/Time.html](https://ruby-doc.org/core-2.7.0/Time.html)
* De 'timecop' gem voor het nabootsen met tijd: [https://github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop)
* De 'chronic' gem voor het parsen van natuurlijke taal datums: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
