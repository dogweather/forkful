---
title:    "C#: Å generere tilfeldige tall"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Dette kan være for å simulere virkelige situasjoner, lage unike nøkler eller for å sikre tilfeldighet i spillet eller lotteriet.

## Slik gjør du det
Det finnes flere måter å generere tilfeldige tall i C# på. En enkel måte er å bruke Random-klassen som finnes i .NET-rammeverket. Her er et eksempel på hvordan dette kan gjøres:

```C#
// Opprett en ny instans av Random-klassen
Random rng = new Random();

// Generer et tilfeldig tall mellom 1 og 100
int randomNumber = rng.Next(1, 101);

// Skriv ut resultatet
Console.WriteLine("Det tilfeldige tallet er: " + randomNumber);
```

En annen måte å generere tilfeldige tall på er å bruke den kryptografisk sikre RandomNumberGenerator-klassen. Denne klassen genererer tall basert på en kryptografisk sikker algoritme og er derfor mer egnet til bruk i sikkerhetskritiske applikasjoner.

```C#
// Opprett en ny instans av RandomNumberGenerator-klassen
RandomNumberGenerator rng = RandomNumberGenerator.Create();

// Opprett et byte-array for å lagre det tilfeldige tallet
byte[] randomNumber = new byte[4];

// Fyll arrayen med tilfeldige bytes
rng.GetBytes(randomNumber);

// Konverter bytes til et heltall og begrens verdien mellom 1 og 100
int randomNumberInt = Math.Abs(BitConverter.ToInt32(randomNumber, 0)) % 100 + 1;

// Skriv ut resultatet
Console.WriteLine("Det tilfeldige tallet er: " + randomNumberInt);
```

## Dypdykk
Det å generere tilfeldige tall kan virke enkelt, men det er viktig å være klar over at alle algoritmer for å generere tilfeldige tall ikke er like sikre. Noen metoder kan være sårbare for såkalt "bias", hvor visse tall har større sjanse for å bli valgt fremfor andre. Derfor er det viktig å bruke riktige klasser og metoder for å sikre tilfeldighet og unngå sikkerhetsrisiko.

## Se også
- [Random-klassen i .NET](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [RandomNumberGenerator-klassen i .NET](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator)
- [Hvordan generere tilfeldige tall i Java](https://blog.opstalent.com/how-to-generate-random-numbers-in-java/)