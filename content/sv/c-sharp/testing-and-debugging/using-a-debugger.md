---
title:                "Att använda en debugger"
aliases:
- /sv/c-sharp/using-a-debugger/
date:                  2024-01-26T03:48:27.575605-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att använda en debugger innebär att man utnyttjar specialverktyg för att testa och diagnostisera kod. Programmerare gör det för att krossa buggar, förstå kodflödet och försäkra sig om att deras kod fungerar som förväntat – det är som att ha ett mikroskop för din kods hjärna.

## Hur man gör:
Tänk dig att du har ett litet program som inte fungerar rätt:

```C#
static void Main()
{
    int resultat = Sum(1, 2);
    Console.WriteLine(resultat);
}

static int Sum(int a, int b)
{
    return a + a; // Hoppsan, borde vara a + b
}
```

Använd debuggern i Visual Studio, sätt en brytpunkt genom att klicka på vänstermarginalen bredvid `return a + a;`. När du kör programmet (med F5) kommer exekveringen att pausa där. Håll muspekaren över variabler för att inspektera deras värden, eller använd Immediate-fönstret för att utvärdera uttryck. Du kommer att se att `a` är 1 och `b` är 2, men `a + a` är inte vår förväntade summa. Ändra det till `a + b`, fortsätt köra (F5), och voilà, konsolen visar ut 3.

## Djupdykning
Historien om felsökning går ända tillbaka till 1940-talet när en riktig bugg (en nattfjäril) hittades i en tidig dator. Dagens debugger, som den i Visual Studio, erbjuder en uppsättning kraftfulla funktioner, inklusive brytpunkter, steg-för-steg exekvering, bevakningsfönster och mer.

Alternativ till Visual Studios debugger inkluderar öppen källkodsalternativ som GDB för C-liknande språk eller pdb för Python, och plattformsoberoende IDE:s som JetBrains Rider eller VS Code som erbjuder debuggingverktyg för C# och andra språk.

När du dyker ned i en debuggers implementation, tittar du på ett program som fästs vid din applikations process. Den tolkar maskinkod, hanterar minnestillstånd och kontrollerar exekveringsflödet. Detta är tung grejer som är avgörande för effektiv felsökning, vilket är varför debug-läge ofta körs långsammare än release-läge där dessa krokar inte finns.

## Se även
- [Dokumentation för Visual Studio Debugger](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [Strategier för felsökning](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
