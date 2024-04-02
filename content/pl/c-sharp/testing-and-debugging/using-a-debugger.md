---
date: 2024-01-26 03:48:15.714827-07:00
description: "Wyobra\u017A sobie, \u017Ce masz ma\u0142y program, kt\xF3ry nie dzia\u0142\
  a prawid\u0142owo: ```C# static void Main() { int result = Sum(1, 2); Console.WriteLine(result);\
  \ } static\u2026"
lastmod: '2024-03-13T22:44:35.412889-06:00'
model: gpt-4-0125-preview
summary: "Wyobra\u017A sobie, \u017Ce masz ma\u0142y program, kt\xF3ry nie dzia\u0142\
  a prawid\u0142owo: ```C# static void Main() { int result = Sum(1, 2); Console.WriteLine(result);\
  \ } static\u2026"
title: Korzystanie z debugera
weight: 35
---

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
