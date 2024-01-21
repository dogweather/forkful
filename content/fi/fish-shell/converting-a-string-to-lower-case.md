---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:38:31.722334-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja Miksi? Kääntäminen merkkijono pieniksi kirjaimiksi tarkoittaa kaikkien kirjainten muuttamista niiden pienikokoisiksi vastineiksi. Ohjelmoijat käyttävät tätä esimerkiksi syötteiden yhdenmukaistamiseen, helpottamaan vertailua ja vähentämään isot ja pienet kirjaimet huomioivat virheet.

## How to:
Kuinka? Fish Shellillä saat merkkijonon pieniksi kirjaimiksi `string lower`-komennolla. Helppoa ja suoraviivaista. Kokeile itse:

```Fish Shell
echo "Tässä On Isolla Alkavia" | string lower
```

Tulostus:

```Fish Shell
tässä on isolla alkavia
```

## Deep Dive
Syväsukellus: Alun perin UNIX-komentotulkit, kuten Bourne shell, eivät tukeneet merkkijonojen käsittelyä suoraan. Fish Shell toi kehittyneet merkkijono-ominaisuudet shellien käyttöön. Vaihtoehtoisia keinoja merkkijonojen pienentämiseen ovat `awk`, `tr`, `sed`, mutta Fishissä `string`-työkalu hoitaa tämän tyylikkäästi. Fish Shell käyttää Unicode-määritelmiä isot-pienet kirjainmuunnoksille, joten esimerkiksi diakriittiset merkit käännetään oikein.

## See Also
Lisätietoja: Tutustu Fish Shell:n dokumentaatioon ja muiden työkalujen käyttöohjeisiin syvällisemmän ymmärryksen saavuttamiseksi.

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html#string)
- [AWK Manual](https://www.gnu.org/software/gawk/manual/gawk.html)
- [GNU `tr` Manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [SED Manual](https://www.gnu.org/software/sed/manual/sed.html)