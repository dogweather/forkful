---
title:                "Kuvion mukaiseen merkkijonon poistaminen."
html_title:           "Elm: Kuvion mukaiseen merkkijonon poistaminen."
simple_title:         "Kuvion mukaiseen merkkijonon poistaminen."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Miksi poistaisit merkkejä, jotka vastaavat tiettyä kuvioa? Tämä on yksi tapa luoda ohjelmia, jotka suodattavat ja järjestävät tietoa. Se on tärkeää, koska se auttaa meitä hallitsemaan suuria tietomääriä tehokkaammin. 

## Miten:
### Poista merkit määritetyn kuvion perusteella:
```Elm
List.filter (\char -> char /= "a") ["a", "b", "c", "a", "d"]
--["b", "c", "d"]
```

### Hae parilliset luvut listasta:
```Elm
List.filter (\num -> modBy 2 num == 0) [1, 2, 3, 4, 5]
--[2, 4]
```

### Lisävinkki:
Voit myös käyttää `not` toimintoa helpottamaan ehtoja. Esimerkiksi:
```Elm
List.filter (not << String.contains "a") ["a", "b", "c", "a", "d"]
--["b", "c", "d"]
```

## Syvempi sukellus:
### Historiallinen tausta:
Poista merkit toiminto on kehitetty jo alkuaikoina, kun ohjelmointikieliä kehitettiin käsittelemään tietoa. Se on ollut käytössä monissa kielissä, kuten Perl, Python ja Ruby.

### Vaihtoehtoisia tapoja:
On olemassa muita tapoja käsitellä tietoja, kuten suodatus, joka käyttää säännöllisiä lausekkeita. Toisaalta, poistamalla merkkejä, jotka vastaavat tiettyä kuvioa, voimme tarkemmin valita, mitkä merkit haluamme poistaa.

### Toteutuksen yksityiskohdat:
Poista merkit toiminto käyttää taustalla korkeamman asteen funktioita, kuten `filter` ja `not`. Nämä toiminnot mahdollistavat koodin tiistikkämpää ja helpommin luettavissa. 

## Katso myös:
- [Official Elm Documentation: Filtering Lists](https://guide.elm-lang.org/lists/filter.html)
- [Stack Overflow: Deleting Characters Matching a Pattern in Elm](https://stackoverflow.com/questions/50559679/deleting-characters-matching-a-pattern-in-elm)