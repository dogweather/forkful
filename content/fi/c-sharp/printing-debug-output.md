---
title:                "Tulostusten debuggaaminen"
html_title:           "C#: Tulostusten debuggaaminen"
simple_title:         "Tulostusten debuggaaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi tulostaa debug-tulosteita? Yksinkertaisesti sanottuna, debug-tulosteiden tulostaminen on hyödyllinen työkalu ohjelmoinnin ongelmien ratkaisemisessa ja koodin virheiden jäljittämisessä.

## Näin teet sen
```C#
// Tulostetaan debug-tuloste
Console.WriteLine("Tämä on debug-tuloste.");

// Tulostetaan muuttujan arvo debug-tulosteena
int numero = 10;
Console.WriteLine("Muuttujan arvo on: " + numero);

// Käytetään string.Format-metodia helpottamaan debug-tulosteen muotoilua
string nimi = "Matti";
int ikä = 25;
string ammatti = "ohjelmoija";
Console.WriteLine(string.Format("Hei, olen {0}, olen {1} vuotta vanha ja minusta tulee isona {2}.", nimi, ikä, ammatti));
```

Alla on esimerkkilähtö ja sen vastaava debug-tuloste:

**Lähtö:**
```
void Main()
{
    int numero = 20;
    Console.WriteLine("Luku on " + numero);
}
```

**Debug-tuloste:**
```
Luku on 20
```

## Syvemmälle debug-tulosteen käyttöön
Debug-tulosteiden tulostaminen on yleinen tapa löytää ohjelmointivirheitä ja korjata niitä. Voit myös käyttää debug-tulosteita tarkistaaksesi, että koodisi toimii halutulla tavalla ja tarkistaa muuttujien arvoja ja väliaikaisia ​​laskelmia. Voit myös tulostaa debug-tulosteita osana virheenkorjausta lisäämällä ne eri kohdissa koodia ja tarkistaaksesi, missä vaiheessa virhe tapahtuu.

## Katso myös
- [MSDN: Debug-tulostaminen C#:ssa](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=netframework-4.8)
- [C# Debug ja Trace luokat](https://www.tutorialsteacher.com/csharp/csharp-debugging)
- [Yleisimmät C# -virheet ja niiden ratkaiseminen](https://code-maze.com/common-errors-csharp-programmer/)