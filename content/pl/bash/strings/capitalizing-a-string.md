---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.232226-07:00
description: "Zmiana pierwszej litery ci\u0105gu na wielk\u0105 w Bashu polega na\
  \ przekszta\u0142ceniu pierwszego znaku ci\u0105gu na wielk\u0105 liter\u0119, pozostawiaj\u0105\
  c reszt\u0119 ci\u0105gu bez zmian.\u2026"
lastmod: '2024-03-13T22:44:35.565784-06:00'
model: gpt-4-0125-preview
summary: "Zmiana pierwszej litery ci\u0105gu na wielk\u0105 w Bashu polega na przekszta\u0142\
  ceniu pierwszego znaku ci\u0105gu na wielk\u0105 liter\u0119, pozostawiaj\u0105\
  c reszt\u0119 ci\u0105gu bez zmian.\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zmiana pierwszej litery ciągu na wielką w Bashu polega na przekształceniu pierwszego znaku ciągu na wielką literę, pozostawiając resztę ciągu bez zmian. Ta technika jest powszechnie stosowana do formatowania wyjścia lub dostosowania się do konwencji kodowania wymagających, aby pewne ciągi rozpoczynały się wielką literą ze względów czytelności lub preferencji stylistycznych.

## Jak to zrobić:

Bash nie posiada wbudowanej funkcji specjalnie do zamiany liter na wielkie, ale można osiągnąć ten efekt, używając rozwinięcia parametrów lub zewnętrznych narzędzi takich jak `awk`. Oto kilka sposobów na zmianę pierwszej litery ciągu na wielką w Bashu:

**Korzystając z rozwinięcia parametru:**

Ta metoda manipuluje ciągiem bezpośrednio w powłoce.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
Wyjście:
```
Hello world
```

**Korzystając z `awk`:**

`awk` to potężne narzędzie do przetwarzania tekstu dostępne w większości systemów operacyjnych typu Unix, które można wykorzystać do zamiany liter na wielkie.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Wyjście:
```
Hello world
```

**Korzystając z `sed`:**

Dla bardziej tradycyjnego podejścia, `sed` może być użyty do zmiany pierwszej litery ciągu na wielką. Jest to jednak nieco bardziej skomplikowane w porównaniu z poprzednimi metodami.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
Wyjście:
```
Hello world
```

Te fragmenty kodu demonstrują, jak zmienić pierwszą literę ciągu na wielką w Bashu, podkreślając elastyczność skryptów powłoki podczas manipulowania tekstem.
