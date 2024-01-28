---
title:                "Refaktorisering"
date:                  2024-01-26T01:17:55.774090-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Refaktorering är processen att omstrukturera befintlig datorprogramkod utan att ändra dess yttre beteende. Programmerare gör det för att rensa upp kod, förbättra läsbarheten, minska komplexiteten och förbättra underhållbarheten.

## Hur:

Låt oss refaktorera en enkel C#-metod som beräknar och skriver ut summan av en array av nummer:

Före refaktorering:
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
        Console.WriteLine("Summan är " + sum);
    }
}
```

Efter refaktorering:
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
        Console.WriteLine($"Summan är {CalculateSum()}");
    }
}

// Användning:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Genom att refaktorera har vi separerat ansvarsområden, gjort `Calculator`-klassen mer flexibel genom att tillåta den ta emot vilken nummerarray som helst, och utnyttjat LINQ för att göra summaberäkningen mer koncis.

## Fördjupning

Refaktorering har sina rötter i programmeringssamhället smalltalk och populariserades under 1990-talet genom Martin Fowlers bok "Refactoring: Improving the Design of Existing Code". Över åren har det blivit en grundläggande del av agila metoder och goda kodningspraxis.

Det finns olika tillvägagångssätt för refaktorering, såsom Red-Green-Refactor inom Test-Driven Development (TDD). Det säkerställer att refaktoreringen inte introducerar buggar genom att starta med ett misslyckat test, få det att passera och sedan städa upp koden.

När man implementerar refaktorering, är det avgörande att ha en omfattande testsvit för att säkerställa att ingen funktionalitet går förlorad under processen. Automatiserade refaktoreringsverktyg, som ReSharper för C#, kan också hjälpa till i denna process genom att erbjuda säkra sätt att ändra kodstrukturer. Dock bör verktyg vara ett komplement till en djup förståelse av kodbasen och kodningsprinciper.

## Se också

- Martin Fowlers grundläggande verk om refaktorering: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Microsofts guide om refaktorering i Visual Studio: [Refaktorering (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- En detaljerad titt på refaktoreringmönster med exempel: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
