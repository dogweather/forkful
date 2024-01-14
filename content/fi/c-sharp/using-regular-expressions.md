---
title:    "C#: Regulaarilausekkeiden käyttö"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllinen työkalu C#-ohjelmointikielen käyttäjille, jotka haluavat tehdä monimutkaisia haku- ja korvaustoimintoja merkkijonojen kanssa. Ne ovat myös erittäin hyödyllisiä tietojenkäsittelyn ammattilaisille ja verkkokehittäjille, jotka haluavat käsitellä suuria määriä tekstiä ja tiedostoja. Säännölliset lausekkeet säästävät aikaa ja vaivaa, kun etsitään tiettyjä kuvioita ja suoritetaan tietyt muutokset merkkijonoihin.

## Kuinka käyttää säännöllisiä lausekkeita C#-ohjelmoinnissa

Säännölliset lausekkeet sisältävät erilaisia toimintoja ja muokkausmahdollisuuksia. Tässä on yksinkertainen esimerkki, jossa korvataan kaikki numerot merkkijonossa niiden vastaavilla kirjaimilla:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string originalString = "1234";
        string replacedString = Regex.Replace(originalString, "[0-9]", "$&A");
        Console.WriteLine("Original string: " + originalString);
        Console.WriteLine("Replaced string: " + replacedString);
    }
}
```

**Tulos:**

```
Alkuperäinen merkkijono: 1234
Korvattu merkkijono: 1A2A3A4A
```

Tässä esimerkissä käytettiin Regex-luokan Replace-metodia korvaamaan kaikki numerot merkkijonossa A-kirjaimella. Regex-luokalla on myös muita hyödyllisiä metodeja, kuten Match ja Split, jotka auttavat löytämään ja erottamaan tietyt merkkijonot käyttäen säännöllisiä lausekkeita.

## Syvempi sukellus säännöllisten lausekkeiden käyttöön

Säännöllisiä lausekkeita voi käyttää monella eri tavalla C#-ohjelmoinnissa. Yksi hyödyllinen sovellus on tietokoneohjelmien lokien käsittely, jossa säännöllisiä lausekkeita voidaan käyttää haku- ja korvaustoimintojen lisäksi myös tiedon erotteluun ja muokkaukseen.

Säännölliset lausekkeet myös tehostavat koodin suorituskykyä verrattuna perinteiseen merkkijonojen käsittelyyn. Ne ovat myös helposti skaalautuvia, joten ne ovat ihanteellisia suurten tekstimäärien käsittelyyn.

On myös tärkeää muistaa, että säännölliset lausekkeet ovat puhtaasti tekstimuotoisia eivätkä ne tue ääkkösiä, joten suomenkielisten merkkijonojen käsittely voi aiheuttaa ongelmia.

## Katso myös

- [Regex-luokan dokumentaatio .NET Frameworkissa (Microsoft)](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [Säännöllisten lausekkeiden opas (Python)](https://docs.python.org/3/library/re.html)
- [Säännöllisten lausekkeiden käyttö tositilanteessa (TutsPlus)](https://code.tutsplus.com/tutorials/8-regular-expressions-you-should-know--net-6149)