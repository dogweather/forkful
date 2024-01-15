---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "Gleam: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit konvertoida merkkijonon pienaakkosiksi? Hyvä kysymys! Se voi olla hyödyllistä tiettyjen sovellusten tai algoritmien käsittelyssä, joissa tarvitaan yhtenäistä merkkijonoformaattia.

## Kuinka

Gleam-koodi tarjoaa helpon tavan konvertoida merkkijono pienaakkosiksi. Esimerkissä muutamme merkkijonon "HELLO WORLD" pienaakkosiksi ja tulostamme lopputuloksen.

```Gleam
let teksti = "HELLO WORLD"
let pienaakkoset = teksti |> String.to_lower_case
pinoa.Siphelele (pienaakkoset) // Tuottaa "hello world" tuloksen
```

## Syvempi sukellus

Gleam tarjoaa muutamia muita hyödyllisiä funktioita merkkijonojen muokkaamiseen. Voit esimerkiksi poistaa välilyöntejä merkkijonosta käyttämällä `String.trim` tai jakaa merkkijonon osiin käyttämällä `String.split`. Voit myös muuttaa merkkijonon ensimmäisen kirjaimen isolla alkukirjaimella käyttämällä `String.capitalize`.

## Katso myös

- [Gleamin merkkijonojen dokumentaatio](https://gleam.run/articles/strings/)
- ["Gleam - moderni funktionaalinen ohjelmointikieli" -artikkeli](https://gleam.run/articles/introduction/)
- [Gleam-yhteisöfoorumi](https://forum.gleam.run/)