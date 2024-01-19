---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Etsiminen ja korvaaminen on keino löytää tiettyä tekstiä koodistasi ja vaihtaa se toiseen. Tämä on hyödyllinen ohjelmoijille, auttaen meitä nopeassa päivittämisessä ja koodin huollossa.

## Kuinka:

Tässä on esimerkkejä siitä, kuinka etsiä ja korvata tekstiä Fish shellissä.

```Fish Shell
# Käytä 'string replace' syntaksia etsimiseen ja korvaamiseen:
string replace 'vanha' 'uusi' $muuttuja

# Esimerkki:
set orig 'Heippa maailma!'
set uusi (string replace 'maailma' 'Fish Shell' $orig)
echo $uusi
```
Tämän ajamisen tuloksena:

```Fish Shell
Heippa Fish Shell!
```

## Syvä sukellus

Fish-shell, joka lanseerattiin vuonna 2005, on moderni vaihtoehto vanhemmille shell-ohjelmille. Etsiminen ja korvaaminen tehdään 'string replace' komennolla. Se on yksinkertainen ja suoraviivainen, mutta puutteellinen monimutkaisten kuvioihin perustuvien korvausten kannalta. Tällaisissa tapauksissa voit käyttää 'sed' tai 'awk' komentoja.

Fish shellin 'string replace' koodin leikkaus on suoraviivaista. Se käyttää internisti strstr()-mekanismia, joka etsii alistringin esiintymistä kohdassa.

## Katso myös

Lisätietoja Fish shellistä ja sen käytöstä voit löytää seuraavista lähteistä:
- Fish Shellin verkkosivusto: [https://fishshell.com/](https://fishshell.com/)
- Fish Shellin dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tutustu GNU:n Sed- ja Awk-komentoihin: [https://www.gnu.org/software/sed/](https://www.gnu.org/software/sed/), [https://www.gnu.org/software/gawk/](https://www.gnu.org/software/gawk/)