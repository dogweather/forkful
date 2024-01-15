---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "C#: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan joutunut käsiksi monimutkaisten merkkijonojen käsittelyyn, tiedät kuinka aikaa vievää ja haastavaa se voi olla. Regular expression eli regex on tehokas työkalu, joka auttaa sinua löytämään ja manipuloimaan merkkijonoja nopeasti ja helposti.

## Miten käyttää regular expressioneja C#:ssa

Regular expressioneja voi käyttää C#:ssa System.Text.RegularExpressions -kirjastolla. Ensimmäiseksi sinun tulee luoda Regex-objekti, joka sisältää säännön, jonka haluat etsiä merkkijonosta.

```C#
// Luodaan Regex-objekti, joka etsii kaikki "koira" merkkijonossa
Regex regex = new Regex("koira");
```

Sitten voit käyttää erilaisia metodeja löytääksesi merkkijonoja, esimerkiksi käyttämällä Match-metodia, joka palauttaa ensimmäisen osuman:

```C#
// Etsitään ensimmäinen osuma Regex-objektilla
Match match = regex.Match("Tänään näin ruskean koiran puistossa.");

// Palauttaa "koira"
string result = match.Value;
```

Voit myös käyttää Replace-metodia korvaamaan osuman toisella merkkijonolla, kuten alla:

```C#
// Korvataan ensimmäinen "koira" merkkijonolla "kissa"
string replaced = regex.Replace("Tänään näin ruskean koiran puistossa.", "kissa");

// Palauttaa "Tänään näin ruskean kissan puistossa."
```

## Syvällinen sukellus

Regular expressioneja voi käyttää myös monimutkaisempien sääntöjen etsimiseen ja manipulointiin, kuten säännöllisten lausekkeiden avulla. Voit myös käyttää säännöistä ryhmiä, joiden avulla voit löytää tiettyjä osia merkkijonosta.

```C#
// Etsitään "koira" sana, joka on seuraavien kolmen sanan jälkeen
Regex regex = new Regex("koira (?<jalkima>", 3);

// Palauttaa "ruskean" kolmen sanan jälkeen
string result = match.Groups["jalkima"].Value;
```

On myös mahdollista käyttää regular expressioneja suorittamaan tiettyjä toimenpiteitä, kuten leikkaamalla merkkijonosta osia tai tarkistamalla löytyykö merkkijonosta tiettyä sanaa.

## Katso myös

- [Microsoftin ohjeet regular expressioneista C#:ssa](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex101 - online-työkalu regular expressioneihin](https://regex101.com/)