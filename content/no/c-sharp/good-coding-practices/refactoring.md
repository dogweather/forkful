---
title:                "Refaktorering"
aliases:
- /no/c-sharp/refactoring/
date:                  2024-01-26T01:17:16.608337-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Refaktorisering er prosessen med å restrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere gjør dette for å rydde opp i koden, forbedre lesbarheten, redusere kompleksiteten og øke vedlikeholdbarheten.

## Hvordan:

La oss refaktorisere en enkel C#-metode som beregner og skriver ut summen av en rekke tall:

Før Refaktorisering:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("Summen er " + sum);
    }
}
```

Etter Refaktorisering:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"Summen er {CalculateSum()}");
    }
}

// Bruk:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Ved å refaktorisere har vi separert bekymringer, gjort `Calculator` klassen mer fleksibel ved å tillate den å ta imot hvilken som helst rekke med tall, og benyttet LINQ for å gjøre sumberegningen mer kortfattet.

## Dypdykk

Refaktorisering har sine røtter i programmeringssamfunnet for Smalltalk, og ble popularisert på 1990-tallet gjennom Martin Fowlers bok "Refaktorisering: Forbedring av designet på eksisterende kode". Gjennom årene har det blitt en grunnleggende del av agile metodikker og god kodingspraksis.

Det finnes ulike tilnærminger til refaktorisering, som Rød-Grønn-Refaktor i Testdrevet Utvikling (TDD). Det sikrer at refaktorisering ikke introduserer feil ved å starte med en test som feiler, få den til å passere, og deretter rydde opp i koden.

Når man implementerer refaktorisering, er det avgjørende å ha et omfattende testsuite for å sikre at ingen funksjonalitet blir ødelagt i prosessen. Automatiserte refaktoriseringsverktøy, som ReSharper for C#, kan også bistå i denne prosessen ved å tilby sikre måter å endre kode strukturer på. Imidlertid bør verktøy være et supplement til en dyp forståelse av kodebasen og kodingsprinsipper.

## Se Også

- Martin Fowlers grunnleggende arbeid om Refaktorisering: [Refaktorisering: Forbedring av designet på eksisterende kode](https://martinfowler.com/books/refactoring.html)
- Microsofts guide om Refaktorisering i Visual Studio: [Refaktorisering (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Et detaljert innblikk i refaktoriseringsmønstre med eksempler: [SourceMaking Refaktorisering](https://sourcemaking.com/refactoring)
