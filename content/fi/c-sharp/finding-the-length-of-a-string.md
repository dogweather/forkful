---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "C#: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Tässä artikkelissa käymme läpi, miten voit laskea merkkijonon eli stringin pituuden C# -ohjelmointikielellä. Stringin pituuden määrittäminen on tärkeä taito, jota tarvitaan monissa eri ohjelmoinnin konteksteissa. Se auttaa meitä esimerkiksi validoimaan käyttäjän syötteet tai käsittelemään tekstidataa oikein.

## Kuinka

Aloita luomalla uusi C# -projekti ja lisää sinne seuraava koodi:
```C#
string sana = "Hei kaikille!";
int pituus = sana.Length;
Console.WriteLine("Merkkijonon pituus on: " + pituus);
```
Tämä koodi luo muuttujan "sana", joka sisältää haluamasi merkkijonon, ja käyttää sille .Length -ominaisuutta laskeakseen sen pituuden. Tulostamme sitten pituuden konsoliin, jolloin näemme tuloksen "Merkkijonon pituus on: 13".

Toinen tapa laskea merkkijonon pituus on käyttää .Count() -metodia seuraavalla tavalla:
```C#
string sana = "Hei kaikille!";
int pituus = sana.Count();
Console.WriteLine("Merkkijonon pituus on: " + pituus);
```
Tämä antaa saman tuloksen kuin ensimmäinenkin esimerkki. Voit myös käyttää .Count() -metodia laskemaan tietyntyyppiset merkit, kuten välilyönnit, seuraavasti:
```C#
string lause = "En halua enää välilyöntejä!";
int valilyonnit = lause.Count(x => x == ' ');
Console.WriteLine("Lauseessa on " + valilyonnit + " välilyöntiä.");
```
Tämä koodi tulostaa "Lauseessa on 4 välilyöntiä.". Käytännössä .Length -ominaisuus ja .Count() -metodi tekevät saman asian, joten voit valita kumman tahansa käyttämisen haluamallasi tavalla.

## Deep Dive

C# tarjoaa meille monia eri tapoja laskea merkkijonon pituus. Voit esimerkiksi käyttää .Substring() -metodia, joka palauttaa halutun osan merkkijonosta annetun aloituskohteen ja pituuden perusteella. Voit käyttää tätä metodia laskemaan merkkijonon pituuden seuraavasti:
```C#
string sana = "Hei kaikille!";
int pituus = sana.Substring(0).Length;
Console.WriteLine("Merkkijonon pituus on: " + pituus);
```
Tässä tapauksessa .Substring(0) palauttaa koko sana-merkkijonon, jonka jälkeen käytämme .Length -ominaisuutta laskeaksemme sen pituuden.

Toinen mielenkiintoinen tapa laskea merkkijonon pituus on käyttää LINQ-kyselyjä. Voit tehdä tämän seuraavasti:
```C#
string sana = "Hei kaikille!";
int pituus = (from c in sana select c).Count();
Console.WriteLine("Merkkijonon pituus on: " + pituus);
```
Tämä tapahtuu käyttämällä LINQ-kyselyä, joka käy läpi jokaisen merkin merkkijonossa ja laskee niiden määrän käyttämällä .Count() -metodia.

## Katso myös

- [C# Merkkijonot (String)](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/strings/)
- [C# .Length -ominaisuus](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.length)
- [C# .Count() -metodi](https