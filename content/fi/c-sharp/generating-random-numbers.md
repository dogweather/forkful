---
title:    "C#: Sattumanvaraisten numeroiden luominen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi: Satunnaislukujen generointi

Miksi kukaan haluaisi generoida satunnaislukuja? Monissa ohjelmoinnin tehtävissä saattaa olla tarvetta käyttää satunnaislukuja, kuten pelilogiikassa, tietokantojen täyttämisessä ja tilastollisissa analyyseissa. Satunnaislukujen avulla voidaan tuottaa erilaisia tuloksia, jotka tekevät ohjelmasta monipuolisemman ja mielenkiintoisemman.

# Kuinka tehdä: Esimerkkejä koodin ja tulosteen kanssa

```C#
// Luodaan satunnaisluku generaattori
Random random = new Random();

// Generoidaan kokonaisluku väliltä 1-10
int number = random.Next(1, 11);
Console.WriteLine("Satunnainen luku väliltä 1-10: " + number);

// Generoidaan desimaaliluku väliltä 0-1
double decimalNumber = random.NextDouble();
Console.WriteLine("Satunnainen desimaaliluku väliltä 0-1: " + decimalNumber);

// Generoidaan satunnainen merkki
char character = (char)random.Next('a', 'z' + 1);
Console.WriteLine("Satunnainen merkki a-z väliltä: " + character);
```

Tulosteenä näemme seuraavaa:

```
Satunnainen luku väliltä 1-10: 7
Satunnainen desimaaliluku väliltä 0-1: 0,456952182543
Satunnainen merkki a-z väliltä: j
```

# Syvällisempää tietoa: Satunnaislukujen generoinnista

Satunnaislukujen generointi perustuu satunnaisluku generaattorin käyttöön, joka tuottaa numeroita tietyn algoritmin avulla. C# tarjoaa kätevän tavan käyttää satunnaisluku generaattoria Random-luokan avulla. Luokalla on useita metodeja, kuten Next(), NextDouble() ja NextBytes(), joilla voidaan generoida erilaisia lukuja.

Satunnaislukujen generoinnissa käytetään usein myös siemenlukua, joka määrittää satunnaislukujen sarjan. Tämä mahdollistaa saman sarjan toistamisen myöhemmin, kunhan käytetään samaa siemenlukua.

Satunnaislukujen generoinnissa on hyvä ottaa huomioon myös niiden jakautuminen tasaisesti eri välillä sekä mahdollisuus toistaa saman arvon generoimista. C# Random-luokassa on joitakin vaihtoehtoja tämän säätämiseen, kuten Seed ja Lock.

# Katso myös

- C# Random-luokka: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0
- Satunnaislukujen generointi pelilogiikassa: https://en.wikipedia.org/wiki/Random_number_generation_in_video_games
- Satunnaislukujen käyttö tilastollisissa analyyseissä: https://en.wikipedia.org/wiki/Random_number_generation_in_statistics