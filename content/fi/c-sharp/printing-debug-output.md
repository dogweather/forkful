---
title:    "C#: Virheenkorjaustulosteen tulostaminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulosteen tulostaminen on tärkeä osa C# ohjelmoinnissa, koska se auttaa havaitsemaan ja korjaamaan virheitä koodissa.

## Miten tehdä

```C#
Console.WriteLine("Tämä on debug-tuloste");
```

Debug-tulosteen tulostaminen C#-koodissa on hyvin yksinkertaista. Käytämme Console-luokkaa ja sen WriteLine-metodia tulostamaan haluamamme viestin. Tätä voidaan käyttää esimerkiksi kohdennettujen muuttujien tai keskeisten arvojen tulostamiseen koodin suorituksen aikana. Tämä auttaa selvittämään, mikä koodin osa aiheuttaa ongelman tai missä vaiheessa virhe tapahtuu.

```C#
int x = 10;
int y = 5;
Console.WriteLine("x:n arvo on: " + x);
Console.WriteLine("y:n arvo on: " + y);
```

Tuloste:

```
x:n arvo on: 10
y:n arvo on: 5
```

Kuten näette, voimme myös yhdistää merkkijonoja ja muuttujia tulosteessa käyttämällä "+" operaattoria.

## Syvempi sukellus

Debug-tulosteen tulostaminen on hyödyllistä myös silloin, kun koodia testataan ja halutaan varmistaa sen toimivuus. Voimme tulostaa tärkeitä tietoja, kuten muuttujan arvoja tai koodipolun suoritusjärjestystä, ja verrata niitä odotettuun tulokseen.

Voimme myös käyttää Debug-luokkaa, joka tarjoaa laajemman valikoiman toimintoja ja ominaisuuksia debug-tulostamiseen. Debug-luokka on osa System.Diagnostics nimiavaruutta ja se toimii samalla tavalla kuin Console-luokka.

```C#
Debug.WriteLine("Tämä on debug-tuloste");
```

Voimme myös määrittää ehtoja tulostamiselle Debug.Assert-metodin avulla:

```C#
Debug.Assert(x > y, "x ei voi olla pienempi kuin y");
```

Tämä tulostaa viestin vain silloin, kun ehto ei täyty.

## Katso myös

- [Microsoft C# Debug.WriteLine dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug.writeline?view=net-5.0)
- [Microsoft C# Debug.Assert dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug.assert?view=net-5.0)
- [C# - Debugging for beginners](https://www.c-sharpcorner.com/article/c-sharp-debugging-for-beginners/)