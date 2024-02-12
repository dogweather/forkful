---
title:                "Korzystanie z debugera"
aliases: - /pl/c-sharp/using-a-debugger.md
date:                  2024-01-26T03:48:15.714827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Używanie debugera oznacza korzystanie ze specjalistycznych narzędzi do testowania i diagnozowania kodu. Programiści robią to, aby wyeliminować błędy, zrozumieć przepływ kodu i upewnić się, że ich kod działa zgodnie z oczekiwaniami – to jak posiadanie mikroskopu dla mózgu kodu.

## Jak to zrobić:
Wyobraź sobie, że masz mały program, który nie działa prawidłowo:

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // Ojej, powinno być a + b
}
```

Używając debugera Visual Studio, ustaw punkt przerwania klikając na lewym marginesie obok `return a + a;`. Gdy uruchomisz program (za pomocą F5), wykonanie zostanie w tym miejscu wstrzymane. Najedź kursorem na zmienne, aby zbadać ich wartości, lub użyj natychmiastowego okna (Immediate Window), aby ocenić wyrażenia. Zobaczysz, że `a` to 1 i `b` to 2, ale `a + a` to nie suma, której się spodziewaliśmy. Zmień to na `a + b`, kontynuuj uruchamianie (F5), i voila, konsola wyświetli 3.

## Dogłębna analiza
Historia debugowania sięga lat 40. XX wieku, kiedy to w jednym z wczesnych komputerów znaleziono prawdziwego robaka (ćmę). Dzisiejsze debugery, takie jak ten w Visual Studio, oferują szereg potężnych funkcji, w tym punkty przerwania, wykonanie krok po kroku, okna obserwacji i wiele innych.

Alternatywy dla debugera Visual Studio obejmują opcje open-source takie jak GDB dla języków stylu C lub pdb dla Pythona, oraz wieloplatformowe IDE takie jak JetBrains Rider czy VS Code, które oferują narzędzia do debugowania dla C# i innych języków.

Gdy zagłębiasz się w implementację debuggera, patrzysz na program, który dołącza do procesu twojej aplikacji. Interpretuje kod maszynowy, zarządza stanem pamięci i kontroluje przepływ wykonania. To ciężka robota, która jest kluczowa dla skutecznego debugowania, dlatego tryb debugowania często działa wolniej niż tryb wydania, gdzie te haczyki nie istnieją.

## Zobacz także
- [Dokumentacja debugera Visual Studio](https://docs.microsoft.com/pl-pl/visualstudio/debugger/)
- [Strategie debugowania](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
