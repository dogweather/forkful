---
title:                "C#: Kuviota vastaavien merkkien poistaminen"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi poistaa merkkejä vastaavat kuvion?

Joskus ohjelmointitehtävissä voi ilmetä tarve poistaa merkkejä tietyllä kuviossa, esimerkiksi tietyt kirjaimet tai numerot. Tämä voi olla tarpeellista esimerkiksi tietojen käsittelyssä tai tietokantojen hallinnassa. Tässä artikkelissa opit kuinka voit helposti poistaa merkkejä vastaavia kuvioita C# -ohjelmointikielen avulla.

## Miten tehdä se?

Poistaaksesi merkkejä vastaavia kuvioita voit käyttää C# string-olioiden Replace() -metodia. Tämä metodi ottaa vastaan kaksi parametria, ensimmäisenä merkkijonon joka halutaan korvata ja toisena korvaavan merkkijonon. Katso alla oleva koodiesimerkki, jossa merkki "a" korvataan tyhjällä merkkijonolla ("").

```C#
string sana = "kissa";
string korvattu = sana.Replace("a", "");
Console.WriteLine(korvattu); // Tulostaa "kiss"
```

Tässä tapauksessa "a" -kirjaimet poistetaan sanasta "kissa" ja saadaan lopputulokseksi "kiss". Voit käyttää tätä metodia myös muilla merkkejä vastaavilla kuvioilla, kuten numeroilla tai välilyönneillä.

Voit myös käyttää Regular Expression -kirjastoa, joka tarjoaa laajemman valikoiman kuvioihin perustuvia hakemistoja ja merkkien poistoja. Alla olevassa koodiesimerkissä käytetään Regular Expression -kirjastoa poistamaan kaikki numerot merkkijonosta.

```C#
using System.Text.RegularExpressions;

string sana = "Puhnro: 123-456-789";
string puhelin = Regex.Replace(sana, @"[0-9]", "");
Console.WriteLine(puhelin); // Tulostaa "Puhnro: --"

```

## Syvemmälle aiheeseen

Tässä artikkelissa esitellyt esimerkit ovat vain pintaraapaisu merkkejä vastaavista kuvioista ja niiden poistamisesta C# -ohjelmointikielellä. On hyvä tutustua myös Regular Expression -kirjaston muihin hyödyllisiin toimintoihin ja syvempiin ominaisuuksiin, kuten capture groups ja regular expression options.

## Katso myös

- [C# Replace() -metodin dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [C# Regular Expression -kirjaston dokumentaatio](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C# string-olion dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)