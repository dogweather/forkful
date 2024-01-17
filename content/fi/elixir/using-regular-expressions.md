---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Elixir: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Säännölliset lausekkeet ovat tärkeä osa monien ohjelmoijien työkalupakkia. Ne ovat tapa hakea, tarkistaa ja muokata tekstiä määritettyjen mallejen avulla. Tämä tekee niistä erittäin hyödyllisiä, kun joudutaan työskentelemään suurten tekstimäärien kanssa tai kun tietyn kuvion etsiminen on välttämätöntä.

## Miten:

Voit käyttää säännöllisiä lausekkeita Elixirissä käyttämällä regex-moduulia. Moduuli tarjoaa lukuisia funktioita, joilla voit luoda säännöllisiä lausekkeita ja suorittaa hakuja. Alla on esimerkkejä, jotka osoittavat, miten voit käyttää säännöllisiä lausekkeita Elixirissä.

```Elixir
# Hae sana "koira" merkkijonosta
Regex.run(~r/koira/, "Minulla on koira kotona") 
# => ["koira"]

# Hae kaikki sanat, joissa on vähintään kolme kirjainta
Regex.scan(~r/[a-z]{3,}/, "Tämä on esimerkki") 
# => [["Tämä"], ["esimerkki"]]

# Korvaa sana "kissa" sanalla "hamsteri" merkkijonossa
Regex.replace(~r/kissa/, "Meillä on kissa talossa", "hamsteri") 
# => "Meillä on hamsteri talossa"
```

## Syvempi sukellus:

Säännölliset lausekkeet ovat olleet tiedejulkaisujen keskuudessa jo vuosikymmeniä ja ne ovat vakiinnuttaneet paikkansa ohjelmoinnissa. Sittemmin on kehitetty erilaisia vaihtoehtoja, kuten muotoilufunktioita ja näihin kohdistuvia kirjastoja. Nämä vaihtoehdot perustuvat usein säännöllisiin lausekkeisiin ja niiden avulla voidaan luoda monimutkaisempia muotoilumalleja.

Säännöllisten lausekkeiden toteutus Elixirissä perustuu POSIX-syntaksiin. Tämä takaa, että Elixirin säännölliset lausekkeet ovat yhteensopivia muiden ohjelmointikielten kanssa, jotka käyttävät samaa syntaksia.

## Katso myös:

- [Elixirin virallinen dokumentaatio säännöllisistä lausekkeista](https://hexdocs.pm/elixir/1.12/Regex.html)
- [Säännöllisten lausekkeiden opas](https://regexone.com/)