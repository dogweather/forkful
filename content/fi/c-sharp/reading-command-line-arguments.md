---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Komentorivin argumenttien lukeminen tarkoittaa käyttäjän syöttämien komentoriviargumenttien nappaamista ohjelman suorituksen aikana. Tämä mahdollistaa ohjelman ajamisen erilaisilla konfiguraatioilla ilman koodin muokkaamista.

## Näin se tehdään:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine("Argumentti[" + i + "]: " + args[i]);
        }
    }
}
```
Jos tämä koodiin käynnistetään komennolla `dotnet run arg1 arg2 arg3`, tulosteena saadaan:

```
Argumentti[0]: arg1
Argumentti[1]: arg2
Argumentti[2]: arg3
```

## Syvällisempi tarkastelu:

- **Historiallinen tausta**: Komentoriviparametrien käyttö juontaa juurensa UNIX-järjestelmästä, jossa komentoriviohjelmilla oli usein monia kytkimiä tai argumentteja.
- **Vaihtoehdot**: Usein argumentit määritellään kiinteästi koodissa, mutta jos ohjelman käyttäjiltä tarvitaan monimutkaisempia tietoja, voidaan käyttää esim. XML- tai JSON-tiedostoa.
- **Implementointi**: Käytännössä `string[] args` -parametri `Main`-metodissa edustaa argumentteja, jotka on annettu ohjelmalle komentoriviltä. Järjestelmä yksinkertaisesti pilkkoo komennon väliintulevilla välilyönneillä ja tallentaa tulokset tähän taulukkoon.

## Katso myös:

- Microsoftin dokumentaatio komentorivin argumenttien käytöstä: https://docs.microsoft.com/en-us/dotnet/core/tutorials/cmdline
- Lisätietoja komentoriviparametrien käyttämisestä: https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/main-and-command-args/