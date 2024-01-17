---
title:                "Satunnaislukujen luominen"
html_title:           "Ruby: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Satunnaisia numeroita generoidaan satunnaislukugeneraattoreilla, jotka ovat algoritmeja, jotka tuottavat luvun, joka ei ole ennustettavissa tai toistettavissa. Tätä tarvitaan monissa ohjelmointitilanteissa, kuten pelien pelaamisessa, salausavaimien luomisessa tai testaamisessa.

## Miten:
Käytä Rubyn `rand`-metodia generoidaksesi satunnaisen kokonaisluvun tai liukuluvun. Voit myös määrittää haluamasi välialueen antamalla parametreiksi luvut, joista lukujen tulee olla. Esimerkiksi `rand(1..10)` generoi luvun väliltä 1-10. Tässä on esimerkki koodista ja sen tuottamasta tuloksesta:

```Ruby
random_number = rand(1..10)
puts random_number
```

Tässä tapauksessa tulostettu luku voisi olla esimerkiksi 7.

## Syväsukellus:
Satunnaislukuja on pyritty generoimaan jo vuosisatojen ajan ja algoritmeja on kehitetty jatkuvasti paremmiksi ja luotettavimmiksi. Monet ohjelmointikielet tarjoavat valmiita satunnaislukufunktioita, mutta ne eivät aina ole yhtä tehokkaita tai luotettavia kuin Rubyn `rand`-metodi.

## Katso myös:
[Random numbers in computer science](https://en.wikipedia.org/wiki/Random_number_generation)