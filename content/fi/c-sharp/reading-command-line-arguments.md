---
title:                "Komentoriviparametrien lukeminen"
html_title:           "C#: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi?

Kirjoittaessamme ohjelmia C# -kielellä, usein joudumme tekemään päätöksiä ja toimimaan sen mukaisesti. Yksi tärkeimmistä päätöksistä on, mitkä syötteet haluamme hyväksyä ohjelmallemme. Tämä johtaa tarpeeseen lukea komentorivin syötteitä, jotta ohjelmamme voi toimia joustavasti ja tehokkaasti.

## Kuinka?

Jos haluat lukea komentorivin syötteitä C# -ohjelmassasi, sinun täytyy ensin luoda Main-metodi, johon annetaan sille parametrit "string [] args". Sitten voit käyttää "foreach" -silmukkaa käydäksesi läpi kaikki syötteet ja suorittaa tarvittavat toimet.

```C#
static void Main(string[] args)
{
    foreach (string arg in args)
    {
        // Tee jotain syötteelle
    }
}
```

Jos haluat tulostaa kaikki syötteet, voit käyttää "Console.WriteLine" -toimintoa sisälle "foreach" -silmukassa. Tulostat syötteen nimen käyttämällä" arg" -muuttujaa.

```C#
foreach (string arg in args)
{
    Console.WriteLine(arg);
}
```

Syötteet lähetetään ohjelmalle välilyönnillä eroteltuna. Voit käyttää "string.Join" -toimintoa yhdistämään kaikki syötteet yhdeksi merkkijonoksi ja tulostaa sen.

```C#
string arguments = string.Join(" ", args);
Console.WriteLine("Syötetyt parametrit: " + arguments);
```

## Deep Dive

Komentorivin syötteet voivat sisältää useita eri parametreja ja niiden arvoja. Jos haluat antaa useita arvoja yhdelle parametrille, voit käyttää välilyöntiä tai erottaa ne toisistaan käyttämällä pilkkua.

```C#
// Komentoriviltä syötetty parametri: -nimi Bob,Anna
// Tulostettu nimi: Bob,Anna
```

Voit myös tarkistaa, onko tiettyä parametria annettu komentoriviltä käyttämällä "Contains" -metodia.

```C#
string name = "Bob";
if (args.Contains("-name " + name))
{
    // Suorita toiminto, joka liittyy parametriin "nimi"
}
```

On myös mahdollista käsitellä virheellisiä syötteitä, kuten puuttuvia parametreja tai väärää tietotyyppiä. Tämä on tärkeää tehdä, jotta ohjelmasi voi suorittaa oikeat toimenpiteet ja välttää mahdollisia kaatumisia.

## Katso myös

- [Microsoftin ohjeet komentoriviparametrien lukemiseen C#-ohjelmissa.](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Kuinka käyttää "foreach" -silmukkaa C# -ohjelmassa.](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/foreach-in)