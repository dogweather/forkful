---
title:                "C#: Regulaarilausekkeiden käyttö"
simple_title:         "Regulaarilausekkeiden käyttö"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Miksi käyttäisit säännöllisiä lausekkeita C# ohjelmoinnissa? Säännölliset lausekkeet ovat hyödyllinen työkalu merkkijonojen käsittelyssä ja validoinnissa. Ne voivat auttaa sinua tarkastelemaan ja muokkaamaan tekstejä nopeasti ja tarkasti.

## Miten

Tässä on yksinkertainen esimerkki C# koodista, joka käyttää säännöllisiä lausekkeita tarkistaaksesi sähköpostiosoitteen formaatin:

```C#
using System;
using System.Text.RegularExpressions;

public class RegularExpressionsExample
{
    public static void Main()
    {
        string email = "example@example.com";
        string pattern = @"^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$"; // säännöllinen lauseke sähköpostiosoitteen validointiin
        Regex regex = new Regex(pattern, RegexOptions.IgnoreCase);

        if (regex.IsMatch(email))
        {
            Console.WriteLine("Sähköpostiosoite on validi.");
        }
        else
        {
            Console.WriteLine("Sähköpostiosoite ei ole validi.");
        }
    }
}
```

**Tulostus:**

```
Sähköpostiosoite on validi.
```

Tässä esimerkissä käytettiin säännöllistä lauseketta tarkastamaan, että sähköpostiosoite koostuu asianmukaisista merkeistä ja seuraavat tietyt formaatit. Voit muokata säännöllistä lauseketta vastaamaan tiettyä vaatimusta, esimerkiksi tietyn pituisen salasanan tarkistamiseen.

## Syvällisempi sukellus

Säännölliset lausekkeet koostuvat erilaisista säännöistä ja operaattoreista, jotka muodostavat mallin merkkijonon tarkastamiseen. Ne voivat olla hyvin hyödyllisiä monimutkaisten merkkijonojen käsittelyssä, mutta niiden käyttöön voi myös liittyä monia haasteita. On tärkeää ymmärtää säännöllisten lausekkeiden syntaksi ja logiikka ennen niiden käyttämistä.

## Katso myös

- [Microsoftin C# säännölliseen lausekkeeseen liittyvä opas](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regular-Expressions.info - resurssi säännöllisten lausekkeiden oppimiseen](https://www.regular-expressions.info/)
- [Learning Regular Expressions by Zed A. Shaw (saatavilla myös suomeksi)](https://regex.learncodethehardway.org/)