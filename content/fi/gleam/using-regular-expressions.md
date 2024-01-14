---
title:    "Gleam: Säännöllisten lausekkeiden käyttö"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttäisimme säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä Gleam-ohjelmoijille, koska ne mahdollistavat tietyn kaavan tai mallin määrittämisen merkkijonoille. Tämä on erityisen hyödyllistä, kun halutaan analysoida ja muokata suuria määriä tietoa, kuten tekstidokumentteja tai HTML-sivustoja. Säännölliset lausekkeet myös säästävät aikaa ja vaivaa, kun samanlaista toimintoa tarvitaan useaan kertaan koodissa.

## Kuinka käyttää säännöllisiä lausekkeita Gleam-ohjelmoinnissa?

Gleamilla on käytössä Perl-yhteensopivat säännölliset lausekkeet, jotka ovat helppokäyttöisiä ja joustavia. Voit aloittaa käyttämällä säännöllisiä lausekkeita "```re```" moduulista, joka on osa Gleam-ydintä. Alla on esimerkki koodista, joka hakee kaikki numerot merkkijonosta ja tulostaa ne:

```Gleam
let input_string = "Viisi merkkiä on enemmän kuin kolme."
let numbers = re.find_all("\\d+", input_string)
io.println(numbers)
```

Tulos:

```Gleam
Some([|Some("Viisi"), None, None, Some("Kolme")|])
```

## Syvempi sukellus säännöllisten lausekkeiden käyttöön

Perusmuotojen lisäksi säännöllisillä lausekkeilla on paljon erilaisia ​​yksityiskohtia ja ominaisuuksia, jotka voivat tehdä niistä tehokkaita työkaluja tiedonkäsittelyyn. Voit esimerkiksi käyttää muuttujia ja summittaisia ilmauksia lisätaksonomian luomiseksi tai käyttää "```|```" -operaatiota etsimään useita erilaisia ​​merkkijonoja yhdellä kertaa. Tutustu Gleam-dokumentaatioon lisätietoja ja esimerkkejä erilaisista säännöllisten lausekkeiden käytöistä.

## Katso myös

- [Säännöllisten lausekkeiden opas Gleam-docsissa](https://gleam.run/documentation/refs/regular_expressions)
- [Säännölliset lausekkeet Perlissä](https://perldoc.perl.org/perlre.html)
- [RegExr - työkalu säännöllisten lausekkeiden testaamiseen ja oppimiseen](https://regexr.com/)