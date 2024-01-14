---
title:                "Gleam: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi etsiä ja korvata tekstejä Gleam-ohjelmoinnilla? No, yksi hyvä syy voisi olla tekstien muokkaaminen ja parantaminen tehokkaammin.

## Kuinka

```Gleam
let teksti = "Tervetuloa Gleamiin!"
let uusi_teksti = teksti |> String.replace("Tervetuloa", "Hello")
IO.print("Uusi teksti: #{uusi_teksti}")
```

Tulostus: "Uusi teksti: Hello Gleamiin!"

Tässä esimerkissä käytetään Gleamin `String` -moduulia etsimään ja korvaamaan tekstiä. Voit vaihtaa tekstejä valitsemalla tekstiriviltä (ensimmäinen parametri) ja korvaamalla sen uudella tekstillä (toinen parametri). Lopuksi tulostetaan uusi teksti käyttämällä Gleamin `IO` -moduulia.

## Syvällinen sukellus

Ehkä olet jo käyttänyt etsimis- ja korvaamistoimintoja muilla ohjelmointikielillä ja haluat tietää enemmän Gleamin `String` -moduulista. Voit löytää lisätietoja virallisesta dokumentaatiosta, jossa kerrotaan tarkemmin `String.replace` -funktiosta ja sen muista toiminnoista.

## Katso myös
- [Gleamin virallinen dokumentaatio (englanniksi)](https://gleam.run/standard-library/string.html)
- [Gleamin GitHub-sivu (englanniksi)](https://github.com/gleam-lang/gleam)
- [Gleamin virallinen Twitter-tili (englanniksi)](https://twitter.com/gleamlanguage)