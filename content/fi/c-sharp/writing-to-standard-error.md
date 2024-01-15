---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "C#: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihminen kirjoittaisi ohjelmaan standardi virheestä?

Kirjoittaminen standardi virheeseen on tärkeä osa ohjelmointia, koska se auttaa löytämään ja korjaamaan virheitä ohjelmassa. Kun ohjelma kohtaa virheen, se kirjoittaa virheilmoituksen standardi virheeseen, joka voidaan sitten lukea ja käsitellä. Tämä auttaa kehittäjiä löytämään ja korjaamaan virheet nopeammin, mikä säästää aikaa ja vaivaa.

## Miten

Koodiesimerkkejä ja näytelähtöä "```C# ... ```" koodilohkoissa.

Kirjoittaminen standardi virheeseen on helppoa C# -ohjelmointikielen avulla. Käytä `Console.Error.WriteLine()` -metodia kirjoitaaksesi haluamasi tekstin standardi virheeseen. Voit myös käyttää `Console.Error.Write()` -metodia, jos haluat kirjoittaa ilman rivinvaihtoa. Alla oleva koodiesimerkki näyttää, kuinka kirjoittaa teksti "Virhe!" standardi virheeseen ja sitten "Success!" standardi tulostukseen.

```C#
Console.Error.WriteLine("Virhe!");
Console.WriteLine("Success!");
```

Tulostus näyttää seuraavalta:

Virhe! 
Success!

## Syvempi sukellus

Kirjoittaminen standardi virheeseen voi olla hyödyllistä myös virheen jäljittämisessä ja debuggaamisessa. Voit lisätä lisätietoja virheilmoitukseen käyttämällä `Console.Error.Write()` -metodia useammin, jolloin saat enemmän tietoa siitä, missä virhe on tapahtunut ja minkä arvojen kanssa ohjelma on toiminut ennen virheen tapahtumista.

Voit myös käyttää `Console.Error.SetOut()` -metodia ohjaamaan standardi virheen tulostus toiseen tiedostoon tai tietovirtaan. Tämä voi olla hyödyllistä, jos haluat tallentaa kaikki virheilmoitukset erilliseen lokitiedostoon.

## Katso myös

- [C# Console-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-5.0) 
- [C# kirjoitusarvot](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/assignment-operator) 
- [Debuggaaminen C# -ohjelmissa](https://docs.microsoft.com/en-us/dotnet/core/tutorials/debugging-with-visual-studio-code)