---
title:                "Fish Shell: Nauhakkeen pituuden etsiminen"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Finnish readers, blogiteksti, Fish Shell ohjelmoinnista

## Miksi

Merkkijonon pituuden etsiminen voi olla tärkeä osa ohjelmointia, jotta voidaan osata käsitellä merkkijonoja eri tavalla tai tehdä tarkkoja laskutoimituksia. Tämän avulla voidaan myös helpottaa datan käsittelyä ja analysointia.

## Kuinka tehdä

Fish Shell tarjoaa helpon tavan löytää merkkijonon pituus käyttäen "string length" komentoa. Alla on esimerkki koodista ja sen tulosteesta:

```Fish Shell
string length "Tervetuloa"
10
```

```Fish Shell
string length "Hei maailma!"
12
```

Voit myös käyttää erilaisia muuttujia ja yhdistää niitä merkkijonoon käyttäen "string concatenation" -komennon avulla:

```Fish Shell
set nimi "Matti"
set sukunimi "Meikäläinen"
echo (string length $nimi$sukunimi)
13
```

## Syvemmälle

Merkkijonon pituuden etsiminen perustuu sille, että jokainen merkki merkkijonossa on oma arvonsa. Tämän avulla Fish Shell pystyy laskemaan kaikkien merkkien summan ja näin löytämään merkkijonon pituuden.

Voit myös käyttää muita komentoja, kuten "string split" tai "string join", jotta voit käsitellä merkkijonoja entistä monipuolisemmin.

## Katso myös

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [String manipulation in Fish Shell](https://fishshell.com/docs/current/cmds/string.html)
- [Community forums for Fish Shell](https://github.com/fish-shell/fish-shell/discussions)