---
title:                "C#: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en viktig del av programmering, spesielt innen spillutvikling og simulering. Ved å bruke tilfeldige tall kan man skape en mer dynamisk og variert opplevelse for brukeren.

## Hvordan

Det finnes flere måter å generere tilfeldige tall i C# på. En enkel metode er å bruke funksjonen `Random()` som returnerer et tilfeldig heltall. For å begrense størrelsen på tallet kan man bruke `Random(max)` hvor `max` er det største tallet som kan genereres.

```C#
// Eksempel på generering av et tilfeldig tall mellom 1 og 10
Random random = new Random(); // Oppretter en instanse av Random-klassen
int randomNumber = random.Next(1, 11); // Bruker Next() metoden med grenser 1 og 11
Console.WriteLine(randomNumber); // Skriver ut det tilfeldige tallet
// Konsollen vil kunne vise et tilfeldig tall mellom 1 og 10, f.eks. 7
```

For å generere flyttall kan man bruke `Random.NextDouble()` som returnerer et tilfeldig tall mellom 0 og 1.

```C#
// Eksempel på generering av et tilfeldig flyttall mellom 0 og 1
Random random = new Random();
double randomDouble = random.NextDouble();
Console.WriteLine(randomDouble);
// Konsollen vil kunne vise et tilfeldig tall, f.eks. 0,5982261
```

En annen måte å generere tilfeldige tall på er å bruke `Guid` klassen som genererer et unikt identifikasjonsnummer. Dette nummeret kan konverteres til et heltall eller flyttall ved hjelp av `Int32.Parse()` eller `Double.Parse()`.

```C#
// Eksempel på generering av et tilfeldig tall fra en Guid
Guid guid = Guid.NewGuid();
int randomNumber = Int32.Parse(guid.ToString().Substring(0, 8));
Console.WriteLine(randomNumber);
// Konsollen vil kunne vise et tilfeldig tall, f.eks. 89172973
```


## Dypdykk

Det er viktig å være oppmerksom på at ingen av metodene over genererer helt tilfeldige tall, men heller pseudotilfeldige tall. Dette betyr at tallene følger et forutsigbart mønster og kan derfor ikke brukes til sikkerhetskritiske formål. For å øke tilfeldigheten kan man sette en såkalt "seed" når man oppretter en instanse av `Random` klassen. Seed'en kan være en hvilken som helst heltallsverdi og vil påvirke rekkefølgen på de tilfeldige tallene som genereres.

```C#
// Eksempel på bruk av seed
Random random = new Random(1234);
```

Man kan også sette en seed basert på systemets klokke for å øke tilfeldigheten. Dette gjøres ved å bruke `Environment.TickCount` som returnerer antall millisekunder siden systemet startet.

```C#
// Eksempel på bruk av environment tick count som seed
Random random = new Random(Environment.TickCount);
```

Ved å blande flere tilfeldighetsgeneratorer, som for eksempel `Random` og `Guid`, kan man få enda mer komplekse og tilfeldige tall.

## Se også

- [Microsoft Dokumentasjon om tilfeldige tall](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [Generering av tilfeldige verdier i C#](https://www.c-sharpcorner.com/UploadFile/mahesh/generate-random-number/)
- [Tilfeldige tall i spillutvikling](https://www.gamasutra.com/blogs/AndrewKirmse/20190822/348767/Game_Dev_Intro_to_Random_Numbers.php)