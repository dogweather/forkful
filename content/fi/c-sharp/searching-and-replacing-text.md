---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "C#: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja miksi? 

Hakeminen ja korvaaminen tarkoittaa tietyn tekstin etsimistä ja muuttamista toiseen tekstiin. Ohjelmoijat tekevät tätä usein, koska se auttaa heitä muokkaamaan ja parantamaan koodiaan nopeasti ja tehokkaasti. 

## Miten: 

Alla on muutamia koodiesimerkkejä, jotka näyttävät, kuinka hakemista ja korvaamista voidaan tehdä käyttäen C# -ohjelmointikieltä. 

```
// Etsi ja korvaa teksti hirviöllä 
string teksti = "Tämä on tekstiä, jossa on sana kissa"; 
string uusiTeksti = teksti.Replace("kissa", "hirviö"); 
Console.WriteLine(uusiTeksti); 
//tulostaa: Tämä on tekstiä, jossa on sana hirviö 

// Etsi ja korvaa tekstiä käyttäen säännöllistä lauseketta 
string teksti = "Tämä on tekstiä, jossa on numero 123"; 
string uusiTeksti = Regex.Replace(teksti, "[0-9]", "X"); 
Console.WriteLine(uusiTeksti); 
//tulostaa: Tämä on tekstiä, jossa on numero XXX 
``` 

## Syvemmälle: 

Hakemisen ja korvaamisen tekniikka on ollut käytössä varhaisista tietokoneista lähtien. Se on erittäin kätevä toiminto, jota voidaan käyttää monissa ohjelmointikielissä, kuten C#. Jos et halua korvata tekstiä, voit myös ainoastaan löytää sen ja saada tiedon sen paikasta. 

## Katso myös: 

- [C# string -luokka](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1) 
- [Regex.Replace -metodi C# -ohjelmointikielessä](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)