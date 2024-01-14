---
title:    "C#: Wyszukiwanie i zamiana tekstu"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, że przeszukiwanie i zamienianie tekstu to częsta i nieodzowna czynność w procesie tworzenia oprogramowania. Czasami należy zmienić tylko jeden mały fragment tekstu, a czasami konieczne jest dokonanie wielu zmian na raz. W takich sytuacjach używanie funkcji wyszukiwania i zamieniania może zaoszczędzić dużo czasu i wysiłku.

## Jak to zrobić

Python oferuje prosty i wydajny sposób na wyszukiwanie i zamienianie tekstu w kodzie. Możesz użyć wbudowanej funkcji `Replace()`, która pozwala na szybką i precyzyjną zmianę tekstu w wybranym obszarze. Przeczytaj poniższy przykład, aby zobaczyć, jak to działa w praktyce:

```C#
string text = "Witaj, świecie!";
string newText = text.Replace("Witaj", "Cześć");
Console.WriteLine(newText);
```

**Output:** Cześć, świecie!

W tym przykładzie zastępujemy wyraz "Witaj" wyrażeniem "Cześć". Funkcja `Replace()` znajduje wszystkie wystąpienia danego tekstu i zamienia je na nowy. To proste rozwiązanie działa także w przypadku, gdy chcemy zmienić wiele fragmentów tekstu. Wystarczy po prostu wywołać funkcję `Replace()` z różnymi parametrami dla każdej potrzebnej zmiany.

```C#
string text = "To jest przykładowy tekst do zamiany";
string newText = text.Replace("jest", "był")
                      .Replace("przykładowy", "inny")
                      .Replace("do", "dla");
Console.WriteLine(newText);
```

**Output:** To aktualnie inny tekst dla zamiany.

## Dogłębne zagłębianie się

Ważne jest, aby zwrócić uwagę na kilka ważnych szczegółów, gdy korzystasz z funkcji wyszukiwania i zamieniania tekstu. Po pierwsze, funkcja ta jest wrażliwa na wielkość liter. To znaczy, że przy zamianie tekstu nie będzie uwzględniana wielkość liter, a więc słowo "Kot" nie zostanie zamienione na "pies". Po drugie, funkcja ta zwróci tylko pierwsze wystąpienie danego tekstu. Jeśli chcesz zamienić wszystkie wystąpienia, musisz użyć funkcji `Replace()` kilka razy lub skorzystać z innej metody.

## Zobacz również

- Dokumentacja Microsoft o funkcji Replace(): https://docs.microsoft.com/pl-pl/dotnet/api/system.string.replace
- Inne sposoby na wyszukiwanie i zamienianie tekstu w C#: https://www.tutorialsteacher.com/csharp/csharp-string-replace