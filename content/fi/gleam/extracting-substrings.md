---
title:                "Gleam: Alimerkkijonojen erottaminen"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi joku haluaisi käyttää substringsin erottelua?

Joskus tarvitset vain tietyn osan merkkijonosta, ei koko merkkijonoa. Substringsin erottelu voi auttaa sinua saamaan tarvitsemasi osan.

## Miten: Koodiesimerkkejä ja tulosteita "```Gleam ... ```" koodilohkojen sisällä.

Esim. haluamme erottaa merkkijonon "Tämä on esimerkki" substringssiin "Tämä" ja "esimerkki":
```
Gleam glee = "Tämä on esimerkki"
file1 = String.substring(glee, 0, 4)
file2 = String.substring(glee, 8, 7)

IO.println(file1)
IO.println(file2)
```
Tuloste:
```
"Tämä"
"esimerkki"
```

## Syvällisempi sukellus: Lisätietoja substringsin erottelusta.

Substringseilla on monia käyttötarkoituksia, kuten esimerkiksi tiedon tallentaminen tietyllä tavalla tai halutun tiedon hakeminen erilaisista tiedostoista. Korkean tason kielet kuten Gleam tekevät substringsien erottelusta helppoa ja tehokasta.

## Katso myös:

- Gleam Dokumentaatio: https://gleam.run/documentation/
- Ohjelmoinnin perusteet: https://ohjelmointitiede.fi/
- Koodiesimerkit substringsien erottelusta: https://gist.github.com/

Kiitos lukemisesta ja onnea substringsien erottelun kokeiluun!