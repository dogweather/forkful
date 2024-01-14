---
title:                "C#: Komentoriviparametrien lukeminen"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentorivivaihtoehtoja

Komentorivivaihtoehtojen lukeminen on tärkeä taito C#-ohjelmoijille, sillä se mahdollistaa ohjelmien dynaamisen toiminnan ja antaa käyttäjille mahdollisuuden muokata ohjelman toimintoja. Tämä on erityisen tärkeää, kun halutaan tehdä monipuolisempia ja interaktiivisempia sovelluksia.

## Näin luet komentorivivaihtoehtoja

Komentorivivaihtoehtojen lukeminen C#-ohjelmassa on helppoa. Komentoriviparametrit tallentuvat ohjelman suorittamisen yhteydessä string-taulukkoon (```string[]```). Ne voi käsitellä ```foreach```-silmukalla tai indeksien avulla.

Esimerkiksi seuraavassa koodissa luetaan komentoriviparametrit ja tulostetaan ne yksi kerrallaan:

```C#
foreach (string arg in args)
{
  Console.WriteLine(arg);
}
```

Jos haluamme, että ohjelma tulostaa viestin, jos parametreja ei anneta, voimme käyttää ```Length```-ominaisuutta tarkastellaksemme taulukon koon:

```C#
if (args.Length == 0)
{
  Console.WriteLine("Komentoriviparametreja ei annettu.");
}
```

## Syvempää tietoa komentorivivaihtoehtojen lukemisesta

Komentoriviparametreilla voidaan myös antaa ohjelmalle erilaisia toimintoja ja arvoja. Esimerkiksi ohjelmaa voi ajaa seuraavasti:

```
dotnet ohjelma.exe -tulosta tiedosto.txt
```

Tässä tapauksessa ohjelma tulostaa tiedoston ```tiedosto.txt``` sisällön. Tämä tapahtuu, koska komentoriviparametreissa määritelty ```-tulosta``` kertoo ohjelmalle, mitä sen tulee tehdä, ja ```tiedosto.txt``` on tässä tapauksessa parametrin arvo.

Komentoriviparametreja voi myös antaa ohjelmalle haluamassaan järjestyksessä. Esimerkiksi edellisen komennon tulostus voitaisiin määritellä myös näin:

```
dotnet ohjelma.exe tiedosto.txt -tulosta
```

Syvempi ymmärrys komentorivivaihtoehtojen lukemisesta auttaa rakentamaan monipuolisempia ja käyttäjäystävällisempiä sovelluksia.

## Katso myös

- [Microsoftin ohjeet komentorivivaihtoehtojen lukemiseen](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Stack Overflow: How to read command line arguments in C#](https://stackoverflow.com/questions/1365407/how-to-read-command-line-arguments-in-c-sharp)