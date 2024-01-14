---
title:    "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi: Miksi voidaan muuntaa merkkijono pieniksi kirjaimiksi?

Merkkijonon muuntaminen pieniksi kirjaimiksi voi olla tarpeellista, kun halutaan vertailla tai käsitellä merkkijonoja yhtenäisellä tavalla. Pienet ja isojen kirjainten ero voi aiheuttaa ongelmia esimerkiksi hakumetodeissa tai tietokantarajoitteissa. Muuntamalla merkkijonot pieniksi kirjaimiksi, ne ovat helpommin vertailtavissa ja käsiteltävissä.

## Näin tehdään: Koodiesimerkkejä ja tulosteita

```Gleam
let string = "Hello World"

let lower_case_string = String.to_lower_case(string)

IO.print(lower_case_string) // tulostaa "hello world"
```

Yllä olevassa koodiesimerkissä nähdään, kuinka merkkijono muunnetaan pieniksi kirjaimiksi käyttämällä Gleamin to_lower_case-funktiota. Korvattu merkkijono tallennetaan uuteen muuttujaan, jotta alkuperäistä muuttujaa ei muuteta.

```Gleam
let string = "HeLlO WoRlD"

let lower_case_string = String.to_lower_case(string)

IO.print(lower_case_string) // tulostaa "hello world"
```

Jos koodissa on jo olemassa merkkijono, jossa on erikokoisia kirjaimia, sevoidaan silti muuntaa pieniksi kirjaimiksi toistamalla sama prosessi.

## Syvemmälle: Lisätietoa merkkijonon muuntamisesta pieniksi kirjaimiksi

Merkkijonon muuntaminen pieniksi kirjaimiksi on tärkeä osa tekstimuokkauspipeliä. Usein kun käsitellään merkkijonoja, halutaan varmistaa, että ne ovat yhtenäisessä muodossa. Gleamin to_lower_case-funktio huolehtii tästä yhtenäisyydestä, ja se on myös erittäin nopea suorittamaan.

On hyvä huomata, että to_lower_case-funktio ei tue kaikkia erikoismerkkejä oikein. Jos mahdollista, kannattaa käyttää Unicode-yhteensopivaa kirjasinta.

## Katso myös

- [Gleamin viralliset dokumentaatiot](https://gleam.run/)
- [Lisätietoa merkkijonojen käsittelystä Gleamissa](https://gleam.run/book/tour/string-handling.html)
- [String-to_lower_case-funktion lähdekoodi](https://github.com/gleam-lang/gleam_stdlib/blob/master/src/String.gleam#L102)