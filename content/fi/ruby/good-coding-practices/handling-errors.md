---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:56:33.425950-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Virheenkäsittelyssä on kyse odottamattoman odottamisesta koodissa – hallitaan virheet ja ongelmat sulavasti ilman, että ohjelma kaatuu. Ohjelmoijat tekevät sitä hallitakseen kulun, kun asiat menevät pieleen ja pitääkseen käyttäjäkokemuksen sujuvana.

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
