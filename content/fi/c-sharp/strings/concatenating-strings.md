---
date: 2024-01-20 17:35:20.082417-07:00
description: "How to (Kuinka): C#:ssa merkkijonojen yhdist\xE4minen onnistuu muutamalla\
  \ eri tavalla. Katsotaan pari esimerkki\xE4."
lastmod: '2024-03-13T22:44:56.565292-06:00'
model: gpt-4-1106-preview
summary: "C#:ssa merkkijonojen yhdist\xE4minen onnistuu muutamalla eri tavalla."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## How to (Kuinka):
C#:ssa merkkijonojen yhdistäminen onnistuu muutamalla eri tavalla. Katsotaan pari esimerkkiä.

### Yhdistäminen '+' operaattorilla:
```C#
string tervehdys = "Hei ";
string nimi = "Maailma";
string viesti = tervehdys + nimi + "!";
Console.WriteLine(viesti); // Tulostaa: Hei Maailma!
```

### Yhdistäminen `string.Concat`-metodilla:
```C#
string tervehdys = "Moro ";
string viesti = string.Concat(tervehdys, "Matti!");
Console.WriteLine(viesti); // Tulostaa: Moro Matti!
```

### Yhdistäminen `StringBuilder`-luokan avulla:
```C#
var builder = new StringBuilder();
builder.Append("Hola ");
builder.AppendLine("Maria!");
builder.AppendFormat("Nyt on {0}.", "maaliskuu");
string kokonaisviesti = builder.ToString();
Console.WriteLine(kokonaisviesti);  
// Tulostaa:
// Hola Maria!
// Nyt on maaliskuu.
```

### Yhdistäminen `$`-merkin (interpolointi) avulla:
```C#
string nimi = "Seppo";
string viesti = $"Heippa {nimi}, miten menee?";
Console.WriteLine(viesti); // Tulostaa: Heippa Seppo, miten menee?
```

## Deep Dive (Sukellus syvemmälle):
Alkujaan C#:ssa merkkijonojen yhdistäminen tehtiin '+' operaattorilla tai `string.Concat`illa. Nämä tavat toimivat, mutta niillä on suorituskyvyn ongelmia suurten merkkijonojen kanssa, koska C#:n merkkijonot ovat muuttumattomia (immutable). Jokaisen liitoksen myötä syntyy uusi merkkijono, mikä voi johtaa tehottomaan muistinkäyttöön.

`StringBuilder`-luokka esiteltiin tarjoamaan ratkaisu tähän ongelmaan. Sen avulla voi rakentaa merkkijonoja tehokkaammin muuttamalla jo olemassa olevaa merkkijonoa sen sijaan, että luodaan uusi jokaisella yhdistämiskerralla.

C# 6.0 toi tullessaan merkkijonojen interpoloinnin ($-merkki), joka tekee koodista selkeämpää ja vähentää tarvittavien liitosten määrää. Interpolointi on nopeaa ja helppokäyttöistä pienillä merkkijonoilla, mutta `StringBuilder` on silti suositeltava vaihtoehto, kun kyseessä ovat suuret ja monimutkaiset merkkijonorakenteet.

## See Also (Katso myös):
1. Microsoft’s official documentation on strings in C#:
   [docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
   
2. .NET Benchmark comparisons for string concatenation:
   [https://dotnetcoretutorials.com/2020/02/06/performance-of-string-concatenation-in-csharp/](https://dotnetcoretutorials.com/2020/02/06/performance-of-string-concatenation-in-csharp/)

3. StackOverflow discussions on StringBuilder vs string concatenation:
   [stackoverflow.com/questions/21078/stringbuilder-vs-string-concatenation-in-c-sharp](https://stackoverflow.com/questions/21078/stringbuilder-vs-string-concatenation-in-c-sharp)
