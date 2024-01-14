---
title:                "C#: Säännöllisten ilmausten käyttö"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita ohjelmoinnissa

Säännölliset lausekkeet ovat voimakas työkalu ohjelmoijille, jotka haluavat etsiä ja muokata tekstiä tehokkaasti. Ne tarjoavat monipuolisen tavan tarkistaa, vastaako annettu merkkijono tietylle kaavalle, joka sitten voidaan käsitellä halutulla tavalla. Tämä tekee niistä erittäin hyödyllisiä esimerkiksi tietokannan kyselyissä tai tekstiprosessoinnissa.

## Kuinka käyttää säännöllisiä lausekkeita C#:lla

Voimme aloittaa säännöllisten lausekkeiden käytön ottamalla käyttöön System.Text.RegularExpressions -kirjaston C#:ssa. Sieltä löydämme Regex-luokan, joka tarjoaa useita hyödyllisiä metodeja säännöllisten lausekkeiden käyttöön. Alla on esimerkki siitä, miten voimme etsiä tiettyjä sanoja tekstistä ja tulostaa ne konsoliin:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string teksti = "Tämä on esimerkkiteksti, josta haluamme löytää tiettyjä sanoja.";
        Regex regex = new Regex(@"\w+");
        MatchCollection matchit = regex.Matches(teksti);

        foreach(Match match in matchit)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```

Tässä esimerkissä käytämme Regex-luokan Matches-metodia, joka etsii kaikki säännöllisen lausekkeen mukaiset merkkijonot annetusta tekstistä ja tallentaa ne MatchCollection-olioon. Sitten voimme käydä läpi nämä matchit ja tulostaa ne konsoliin.

## Syvempää tietoa säännöllisistä lausekkeista

Säännölliset lausekkeet voivat olla monimutkaisia ja niiden käyttö voi vaatia paljon opiskelua ja harjoittelua. On myös tärkeää huomata, että eri ohjelmointikielissä säännölliset lausekkeet voivat vaihdella hieman, joten kannattaa tarkistaa käyttämäsi kielen dokumentaatio säännöllisiin lausekkeisiin liittyen.

Kehittyneempien säännöllisten lausekkeiden käyttöön C#:ssa voit tutustua esimerkiksi RegexOptions-luokkaan, joka tarjoaa erilaisia asetuksia säännöllisten lausekkeiden käyttöön. Voit myös käyttää säännöllisiä lausekkeita esimerkiksi tiedon etsimiseen tietokannoista tai tekstien muokkaukseen.

## Katso myös

- Säännölliset lausekkeet C#:ssa Microsoftin dokumentaatiosta: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference
- Koodiesimerkkejä C#:ssa säännöllisten lausekkeiden käytöstä: https://www.dotnetperls.com/regex
- Lyhyt opas säännöllisten lausekkeiden käyttöön eri ohjelmointikielillä (englanniksi): https://www.regular-expressions.info/tutorial.html