---
title:                "Alimerkkijonojen erottelu"
html_title:           "Gleam: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# Mitä & miksi?

Substringien erottaminen on prosessi, jossa ohjelmoijat etsivät merkkijonosta tietyt osat, jotka vastaavat määritettyjä kriteerejä. Useimmiten tätä käytetään tiedon jäsentämiseen ja käsittelyyn, jotta voidaan helpommin käsitellä suuria määriä dataa. Substringien erottaminen on tärkeä työkalu monissa ohjelmointitehtävissä, ja sen avulla voidaan tehdä monimutkaisista tehtävistä helpommin hallittavia.

# Miten?

Gleamilla on monia tapoja toteuttaa substringien erottamista. Yksi yleisimmistä on käyttää funktiota `substring` yhdessä `find`-funktion kanssa, joka etsii merkkijonosta tietyt kohdat. Tässä esimerkissä otamme luvun 123 merkkijonosta "Gleam on ohjelmointikieli", ja palauttaa uuden merkkijonon "ohjelmointikieli" käyttämällä `substring`-funktiota yhdessä `find`-funktion kanssa:

```Gleam
gleam> let s = "Gleam on ohjelmointikieli"
gleam> let i = find(s, "123")
Option(10)
gleam> let substr = substring(s, i, 17)
gleam> substr
"Ohjelmointikieli"
```

# Syvällisemmin

Substringien erottamista on käytetty ohjelmointitehtävissä jo pitkään, mutta Gleamilla sen toteuttaminen on helpompaa ja kevyempää kuin perinteisillä ohjelmointikielillä. Lisäksi Gleam tarjoaa myös muita vaihtoehtoja kuten `match`-lausekkeen, joka mahdollistaa monimutkaisempien substringien erottamisen. Implementationäpitä levetä käyttämällä `pattern`-avainsanaa ja ohjaamalla kunkin substritudun kohdan oikealle muuttujalle.

# Katso myös

Käy lukemassa lisää Gleamista ja sen ominaisuuksista sen viralliselta verkkosivulta osoitteesta www.gleam-lang.org. Lisätietoja substringien erottamisesta löydät Gleamin dokumentaatiosta osoitteesta www.gleam-lang.org/docs. Voit myös tarkastella muita vaihtoehtoja, kuten Regex-kirjastoa, joka tarjoaa lisää työkaluja merkkijonojen hallintaan.