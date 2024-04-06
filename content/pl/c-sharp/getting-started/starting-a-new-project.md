---
date: 2024-01-20 18:03:05.413806-07:00
description: "How to: (Jak to zrobi\u0107:) Historia narz\u0119dzi tworzenia projekt\xF3\
  w w C# si\u0119ga IDE Microsoft Visual Studio, kt\xF3re zrewolucjonizowa\u0142o\
  \ spos\xF3b, w jaki deweloperzy\u2026"
lastmod: '2024-04-05T22:50:49.728196-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Historia narz\u0119dzi tworzenia projekt\xF3w w C#\
  \ si\u0119ga IDE Microsoft Visual Studio, kt\xF3re zrewolucjonizowa\u0142o spos\xF3\
  b, w jaki deweloperzy rozpoczynaj\u0105 prace."
title: Rozpoczynanie nowego projektu
weight: 1
---

## How to: (Jak to zrobić:)
```C#
// Tworzenie nowego projektu konsoli w C# z użyciem .NET CLI
// Otwórz terminal i wpisz:
dotnet new console -o MojaAplikacja

// Przejdź do katalogu projektu:
cd MojaAplikacja

// Uruchom aplikację:
dotnet run

// Powinieneś zobaczyć:
Hello, World!
```

## Deep Dive (Dogłębna Analiza)
Historia narzędzi tworzenia projektów w C# sięga IDE Microsoft Visual Studio, które zrewolucjonizowało sposób, w jaki deweloperzy rozpoczynają prace. Alternatywą dla linii poleceń jest właśnie Visual Studio, gdzie nowy projekt tworzy się przez wizualne interfejsy.

W CLI (.NET Core CLI, obecnie po prostu .NET CLI) używamy `dotnet new` z odpowiednimi szablonami. Dla konkretnych rodzajów projektów (np. aplikacje webowe, biblioteki klas) są inne szablony. Warto też wspomnieć o opcjach konfiguracyjnych dostępnych w pliku projektu (.csproj), które umożliwiają szczegółowe dostosowanie budowania i uruchamiania projektu.

Podczas tworzenia projektu ważne jest zrozumienie struktury folderów i plików oraz konwencji nazewnictwa. Jeśli dobrze zorganizujemy projekt na początku, łatwiej będzie go rozwijać.

## See Also (Zobacz Również)
- [Dokumentacja .NET CLI](https://docs.microsoft.com/pl-pl/dotnet/core/tools/)
- [C# Programming Guide (Przewodnik programowania w C#)](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/)
