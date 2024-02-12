---
title:                "Koodin refaktorointi"
aliases:
- /fi/ruby/refactoring.md
date:                  2024-01-26T03:36:57.422645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia rakenneuudistetaan muuttamatta sen ulkoista toimintaa. Ohjelmoijat refaktoroivat parantaakseen ohjelmiston ei-toiminnallisia ominaisuuksia, kuten luettavuutta, vähennettyä monimutkaisuutta, parantunutta ylläpidettävyyttä tai suorituskyvyn parantamista.

## Kuinka:

Käydään läpi esimerkki Ruby-metodin refaktoroinnista, joka laskee neliöiden summan.

**Ennen refaktorointia:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Tuloste: 14
```

**Refaktoroinnin jälkeen:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Tuloste: 14
```

Refaktoroitu versio käyttää Ruby Enumerablesia ilmaistakseen saman logiikan tiiviimmin ja selkeämmin. `map`-metodi muuntaa jokaisen elementin ja `sum` aggregoi niiden arvot, poistaen tarpeen manuaaliselle silmukoiden hallinnalle ja muuttujan sijoituksille.

## Syvä sukellus

Refaktoroinnilla on rikas historiallinen konteksti, joka juontaa juurensa ohjelmistokehityksen alkuaikoihin. Ensimmäiset maininnat voidaan jäljittää 1990-luvulle, merkittäviä panoksia teki Martin Fowler kirjassaan "Refactoring: Improving the Design of Existing Code", missä hän tarjoaa katalogin kuviot refaktorointiin. Siitä lähtien refaktorointi on muodostunut ketterän kehityksen kulmakiveksi.

Puhuessamme refaktoroinnin vaihtoehdoista, meidän on harkittava erilaista lähestymistapaa kuten "Uudelleenkirjoittaminen", missä korvaat vanhan järjestelmän osittain tai kokonaan tai adaptoit käytänteitä kuten "Koodiarvostelut" ja "Pariohjelmointi" parantaaksesi koodin laatua asteittain. Kuitenkaan nämä eivät ole korvikkeita refaktoroinnille; ne täydentävät prosessia.

Toteutuksen kannalta Ruby tarjoaa erinomaisen ja ilmaisuvoimaisen syntaksin, joka usein johtaa lyhyempään, luettavampaan koodiin refaktoroinnin jälkeen. Keskeisiä periaatteita ovat DRY (Don't Repeat Yourself), merkityksellisten nimien käyttäminen, metodien pitäminen lyhyinä ja keskittyneinä yhteen tehtävään, ja Rubyn Enumerable-moduulin tehokas käyttö, kuten yllä olevassa esimerkissä nähdään. Automaattiset työkalut, kuten RuboCop, voivat myös auttaa ohjelmoijia tunnistamaan koodin kohdat, jotka hyötyisivät refaktoroinnista.

## Katso myös

Syventyäksesi Ruby-koodin refaktorointiin, tutustu näihin resursseihin:

- Martin Fowlerin perusteos: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Rubyn tyyliopas puhtaamman koodin kirjoittamiseen: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, staattinen koodianalysaattori (lintteri) ja muotoilija: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
