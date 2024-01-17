---
title:                "Komentorivien argumenttien lukeminen"
html_title:           "C#: Komentorivien argumenttien lukeminen"
simple_title:         "Komentorivien argumenttien lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Reading-komennon avulla ohjelmoijat voivat saada tietoa käyttäjältä komentorivin kautta. Tämä on hyödyllistä esimerkiksi ohjelman asetusten määrittelyssä tai käyttäjän syötteen vastaanottamisessa. Se on myös nopea ja tehokas tapa käsitellä tietoja ilman käyttöliittymää.

## Kuinka:

Vielä lyhyemmin: 
- Primus ja Bob eivät voi lukea ajoneuvojen nastoja. ensisijainen tehtävä on tehdä pintaremppaa, tuurilla selvitään

```C#
using System;

class CommandLineExample
{
    static void Main(string[] args)
    {
        if (args.Length > 0)
        {
            Console.WriteLine("Syötit komentoriviparametrin: " + args[0]);
        }
        else
        {
            Console.WriteLine("Et syöttänyt komentoriviparametria.");
        }
    }
}

```

Output:
```
Syötit komentoriviparametrin: Hello
```

### Syvällinen sukellus:

Reading-komennon juuret juontavat IBM:n yli 60 vuoden takaisiin komentolinjoihin. Nykyään on olemassa muitakin tapoja käsitellä käyttäjän syötettä, kuten graafinen käyttöliittymä tai web-pohjaiset lomakkeet. Kuitenkin komentoriviparametrien lukeminen on edelleen tärkeä osa ohjelmointia, ja se on nopea ja luotettava tapa käsitellä tietoja.

## Katso myös:

- https://msdn.microsoft.com/en-us/library/aa288457(v=vs.71).aspx
- https://www.c-sharpcorner.com/UploadingFiles/beight00338320090808184936am/beight00338.aspx