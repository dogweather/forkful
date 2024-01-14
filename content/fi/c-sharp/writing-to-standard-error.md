---
title:                "C#: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi ohjelmoijat saattavat haluta kirjoittaa standardi virheeseen. Ehkä he haluavat seurata ohjelmansa suorittamista ja korjata mahdollisia virheitä. Ehkä he haluavat tallentaa tietoa suorituksen aikaisten ongelmien ilmenemisestä. Joten siirrytään kirjoittamaan!

## Kuinka tehdä

Käytä C#:n ```Console``` luokan ```WriteLine()``` -metodia kirjoittaaksesi viestin toiseen pääteasteikkoon (tavallisesti terminaalina tunnetaan). Tässä on esimerkki siitä, kuinka voimme käyttää sitä:

```C#
Console.Error.WriteLine("Tämä on virheviesti!");

// Output:
// Tämä on virheviesti!
```

Mutta mitä tapahtuu, jos haluamme kirjoittaa muuttuvan arvon standardi virheeseen? Ei hätää, voimme yksinkertaisesti muuttaa merkkijonon osaa käyttämällä ```+``` -operaattoria, kuten tässä:

```C#
int luku = 42;
Console.Error.WriteLine("Valitsemasi numero on: " + luku);

// Output:
// Valitsemasi numero on: 42
```

Näin meillä on nyt kyky kirjoittaa muuttuvan arvon standardi virheeseen ja säilyttää ohjelman suorituksen seuranta!

## Syväsukellus

Mutta mitä tapahtuu, jos haluamme lukea standardi virheestä kirjoittamamme viestit? Tässä tapauksessa voimme käyttää ```Console``` luokan ```OpenStandardError()``` -metodia avataksemme pääteasteikon, joka tulee sisältämään kaikki kirjoittamamme viestit. Tässä on esimerkki:

```C#
var virheViestit = Console.OpenStandardError();
var luetutViViestit = new byte[virheViestit.Length];
virheViestit.Read(luetutViViestit, 0, (int)virheViestit.Length);

Console.WriteLine(Encoding.UTF8.GetString(luetutViViestit));

// Output:
// Tämä on virheviesti!
// Valitsemasi numero on: 42
```

Kuten näette, standardi virheestä lukeminen voi olla hyödyllistä, jos haluamme tallentaa tai näyttää ne jossakin muodossa. Kannattaa kokeilla ja löytää oma tapasi käyttää standardi virhettä!

## Katso myös

- [C# DokuWiki - Standardi-virheen kirjoittaminen](https://www.c-sharpcorner.com/uploadfile/a25be7/console-class-in-C-Sharp/)
- [Microsoftin viralliset dokumentaatiot - Console-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-5.0)