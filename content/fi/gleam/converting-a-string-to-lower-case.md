---
title:                "Gleam: Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? On olemassa monia syitä, kuten tiedon normalisointi, käyttäjän syötteen vakioiminen tai mahdollisten bugien välttäminen suur- ja pienikirjaimien sekoittumisessa.

## Kuinka

Kääntäminen merkkijonosta pieneksi on yksinkertaista Gleam-ohjelmointikielellä. Sinun tarvitsee vain käyttää Funktioa `String.to_lower`. Tässä on yksinkertainen esimerkki koodinpätkä, joka muuttaa annetun merkkijonon pieniksi kirjaimiksi ja tulostaa sen:

```
Gleam
pub fn main() {
  let merkkijono = "Tämä ON EtSITTy MERKKIJONo"
  let uusi_merkkijono = String.to_lower(merkkijono)
  io.println(uusi_merkkijono)
}

// Output: "tämä on etsitty merkkijono"
```

Gleamissä on myös mahdollista muuttaa merkkijono suuriksi kirjaimiksi käyttämällä Funktiota `String.to_upper`.

## Syvällinen sukellus

Kun käytät Funktiota `String.to_lower`, Gleam muuttaa merkkijonon Unicode-merkistön mukaisesti pieniksi kirjaimiksi. Tämä eroaa monista muista ohjelmointikielistä, jotka luottavat ASCII-aakkoston muuttamiseen.

Lisäksi Gleamillä on myös funktiot `String.to_title_case` ja `String.to_capital_case`, jotka muuttavat merkkijonon alkukirjaimen tai jokaisen sanan ensimmäisen kirjaimen suureksi.

## Katso myös

- [Gleamin virallinen sivusto](https://gleam.run/)
- [Gleamin dokumentaatio](https://gleam.run/book/index.html)
- [Gleamin Github-repositorio](https://github.com/gleam-lang/gleam)