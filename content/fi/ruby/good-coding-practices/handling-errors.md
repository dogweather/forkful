---
date: 2024-01-26 00:56:33.425950-07:00
description: "Kuinka: Ruby k\xE4ytt\xE4\xE4 `begin`, `rescue`, `ensure` ja `end` avainsanoja\
  \ virheenk\xE4sittelyss\xE4. K\xE4\xE4rit riskaabelin koodin `begin` ja `end` rakenteen\
  \ sis\xE4\xE4n.\u2026"
lastmod: '2024-03-13T22:44:57.094408-06:00'
model: gpt-4-1106-preview
summary: "Ruby k\xE4ytt\xE4\xE4 `begin`, `rescue`, `ensure` ja `end` avainsanoja virheenk\xE4\
  sittelyss\xE4."
title: "Virheiden k\xE4sittely"
weight: 16
---

## Kuinka:
Ruby käyttää `begin`, `rescue`, `ensure` ja `end` avainsanoja virheenkäsittelyssä. Käärit riskaabelin koodin `begin` ja `end` rakenteen sisään. Jos virhe tapahtuu, `rescue` aktivoituu.

```Ruby
begin
  # Riskialtis koodi tulee tänne.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Hups! Et voi tehdä tuota: #{e.message}"
ensure
  puts "Tämä suoritetaan aina, olipa virhettä tai ei."
end
```

Esimerkkituloste:
```
Hups! Et voi tehdä tuota: jaettu nollalla
Tämä suoritetaan aina, olipa virhettä tai ei.
```

## Syväsukellus
Historiallisesti ohjelmointikielten virheenkäsittely on kehittynyt merkittävästi, ja varhaisilla kielillä oli usein alkeellisia tai olemattomia mekanismeja. Rubyn poikkeuskäsittely on inspiroitunut kielistä kuten Python ja Smalltalk.

Vaihtoehtoja `begin-rescue` -rakenteelle Rubyssa on käyttää `rescue`-sanaa metodeissa tai käyttää `throw` ja `catch` komentoja ei-standardeissa ohjelmankulun hallinnassa, mutta niitä ei käytetä tyypilliseen virheenkäsittelyyn.

Yksi mielenkiintoinen yksityiskohta: Rubyn poikkeukset ovat olioita (Exception-luokan ja sen alaluokkien instansseja), joten voit määrittää omia virheluokkia ja tehdä enemmän kuin vain kirjata virheitä – voit kuljettaa rikasta tilaa ympäri ohjelmaa robustimpaan virheenkäsittelyyn.

## Katso myös
- Rubyn dokumentaatio poikkeuksista ja virheenkäsittelystä: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Yksityiskohtainen opas Rubyn virheenkäsittelyn parhaista käytänteistä: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
