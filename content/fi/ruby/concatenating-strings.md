---
title:    "Ruby: Jonojen yhdistäminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Ruby on dynaaminen ohjelmointikieli, joka antaa mahdollisuuden monipuoliseen ja tehokkaaseen koodaamiseen. Yksi käyttökelpoinen toiminto Rubyssa on tekstien eli stringien yhdistäminen, jolla voi yksinkertaisesti muodostaa uusia merkkijonoja. Tässä artikkelissa tarkastellaan, miten stringien konkatenointi eli yhdistäminen tapahtuu Rubyssa ja miksi se on hyödyllistä.

## Miten

Stringien konkatenointi on helppoa ja nopeaa Rubyssa. Se tapahtuu käyttämällä plus-merkkiä yhdistämään kaksi tai useampaa merkkijonoa. Alla on esimerkki, jossa konkatenoidaan kaksi eri stringiä:

```Ruby
puts "Tervetuloa " + "koodaamaan!"
```

Tämän koodin tulosteena näkyy "Tervetuloa koodaamaan!". Lisäksi Rubyssa on myös käytössä lyhyempi ja käytännöllisempi tapa yhdistää stringejä, käyttämällä plus-merkkiä ja yhtäsuuruusmerkkiä:

```Ruby
string1 = "Hei "
string2 = "sinä!"
puts string1 += string2
```

Tämä koodi tulostaa saman lopputuloksen kuin ensimmäinen esimerkki. Tässä tapauksessa käytetään muuttujia, joiden arvot yhdistetään plus-merkillä ja tallennetaan uuteen muuttujaan.

## Syvemmälle

Rubyssa stringien konkatenointi tapahtuu takana olevassa koodissa käyttäen `+`-operaattoria, joka suorittaa yhdistämisen. Tämä tarkoittaa, että jokainen uusi stringi luodaan aina erillisenä oliona, vaikka se muodostettaisiinkin vanhojen stringien pohjalta. Tämä on tärkeää pitää mielessä, jos tarvitset yhdistettyjä stringejä edelleen jossakin muussa osassa koodia.

Ruby myös tukee muuttuvia määrityksiä stringien yhdistämiselle käyttäen `<<`-operaattoria. Tämä tarkoittaa, että voit muokata alkuperäisiä stringeja suoraan ilman uuden muuttujan luomista. Esimerkiksi:

```Ruby
string1 = "Tämä on "
string2 = "esimerkki."
puts string1 << string2
puts string1 #Uusi stringi tallennettu alkuperäiseen muuttujaan
```

Tämän koodin tulosteena näkyy sama lopputulos kuin aiemmin. Huomaa kuitenkin, että string1 on muuttunut ja tallentaa nyt uuden yhdistetyn stringin.

## Katso myös

- [RubyDocumentation: String](https://ruby-doc.org/core-2.6.5/String.html)
- [Concatenation in Ruby](https://www.geeksforgeeks.org/concatenation-in-ruby/)

Stringien yhdistäminen on vain yksi esimerkki siitä, kuinka Rubyssa voi käytännöllisesti ja tehokkaasti työskennellä ja manipuloida tekstejä. Toivottavasti tämä artikkeli auttaa sinua ymmärtämään tätä toimintoa paremmin ja hyödyntämään sitä koodissasi. Onnea koodaamiseen!