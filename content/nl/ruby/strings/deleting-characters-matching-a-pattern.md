---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:13.077490-07:00
description: "Het verwijderen van tekens die overeenkomen met een patroon in strings\
  \ gaat over het nauwkeurig verwijderen van delen die je niet nodig hebt, zoals het\u2026"
lastmod: 2024-02-19 22:05:10.408151
model: gpt-4-0125-preview
summary: "Het verwijderen van tekens die overeenkomen met een patroon in strings gaat\
  \ over het nauwkeurig verwijderen van delen die je niet nodig hebt, zoals het\u2026"
title: Karakters verwijderen die overeenkomen met een patroon
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van tekens die overeenkomen met een patroon in strings gaat over het nauwkeurig verwijderen van delen die je niet nodig hebt, zoals het strippen van hashtags uit tweets. Programmeurs doen dit om gegevens op te schonen, consistent te formatteren of ze voor te bereiden op verdere verwerking.

## Hoe te:
```Ruby
# Eenvoudige verwijdering met String#gsub
voorbeeld = "Hallo, #Wereld!"
schoon_voorbeeld = voorbeeld.gsub(/#/, '') # => "Hallo, Wereld!"

puts schoon_voorbeeld # Uitvoer: Hallo, Wereld!

# Het verwijderen van een reeks tekens
reeks_voorbeeld = "D1t is 2 een voorbeeld3."
schoon_reeks = reeks_voorbeeld.gsub(/[0-9]/, '') # => "Dit is een voorbeeld."

puts schoon_reeks # Uitvoer: Dit is een voorbeeld.

# Verwijderen met String#delete
verwijder_voorbeeld = "Verwijder klinkers uit deze zin."
schoon_verwijder = verwijder_voorbeeld.delete('aeiou') # => "Vrwjdr klnkrs t dz zn."

puts schoon_verwijder # Uitvoer: Vrwjdr klnkrs t dz zn.
```

## Diepgaand Onderzoek
Historisch gezien is Ruby een taal geweest met een sterke focus op tekstverwerking, waarbij het enkele van zijn filosofieën van Perl heeft overgenomen. Daarom geeft het je gereedschappen zoals `gsub` en `delete` direct uit de doos.

`gsub` staat voor globale vervanging. Het wordt vaak gebruikt om delen van strings die overeenkomen met een patroon (reguliere expressie) te vervangen door een andere string. Wanneer het een lege vervangingsstring krijgt, verwijdert het effectief de overeenkomende karakters.

`delete` is minder flexibel dan `gsub`, maar sneller wanneer je gewoon specifieke karakters wilt verwijderen. Je kunt geen reguliere expressies gebruiken met `delete`, maar voor eenvoudige karaktersverwijdering is het de voor de hand liggende keuze.

Er zijn echter andere manieren om deze kat te villen. Bibliotheken als `scan` en `split` kunnen strings ontleden, en je kunt ze vervolgens weer in elkaar zetten zonder de ongewenste karakters. Maar voor het direct verwijderen van karakters zijn `gsub` en `delete` je beste maatjes.

## Zie Ook
- Ruby's `gsub` documentatie: [Ruby Doc gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- Ruby's `delete` documentatie: [Ruby Doc delete](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- Regelmatige Expressies in Ruby: [Ruby Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- "Programming Ruby: The Pragmatic Programmer’s Guide" voor een diepgaande blik op Ruby's tekstverwerkingscapaciteiten.
