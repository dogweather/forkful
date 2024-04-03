---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:02.315407-07:00
description: "Jak to zrobi\u0107: W przeciwie\u0144stwie do niekt\xF3rych j\u0119\
  zyk\xF3w, kt\xF3re maj\u0105 wbudowan\u0105 interpolacj\u0119 ci\u0105g\xF3w, VBA\
  \ wymaga bardziej r\u0119cznego podej\u015Bcia, zazwyczaj za\u2026"
lastmod: '2024-03-13T22:44:35.215178-06:00'
model: gpt-4-0125-preview
summary: "W przeciwie\u0144stwie do niekt\xF3rych j\u0119zyk\xF3w, kt\xF3re maj\u0105\
  \ wbudowan\u0105 interpolacj\u0119 ci\u0105g\xF3w, VBA wymaga bardziej r\u0119cznego\
  \ podej\u015Bcia, zazwyczaj za pomoc\u0105 operatora `&` lub funkcji `Format` do\
  \ osadzania zmiennych w ci\u0105gach."
title: "Interpolacja ci\u0105gu znak\xF3w"
weight: 8
---

## Jak to zrobić:
W przeciwieństwie do niektórych języków, które mają wbudowaną interpolację ciągów, VBA wymaga bardziej ręcznego podejścia, zazwyczaj za pomocą operatora `&` lub funkcji `Format` do osadzania zmiennych w ciągach. Poniżej znajdują się przykłady pokazujące te metody:

**Korzystając z operatora `&`:**

```vb
Dim userName As String
Dim userScore As Integer

userName = "Alice"
userScore = 95

' Łączenie ciągów i zmiennych
Dim message As String
message = "Gratulacje, " & userName & "! Twój wynik to " & userScore & "."
Debug.Print message
```
**Wynik:**
```
Gratulacje, Alice! Twój wynik to 95.
```

**Korzystając z funkcji `Format`:**

W bardziej złożonych scenariuszach, takich jak uwzględnianie sformatowanych liczb lub dat, funkcja `Format` jest nieoceniona.

```vb
Dim currentDate As Date
currentDate = Date

Dim formattedMessage As String
formattedMessage = "Dzisiaj jest " & Format(currentDate, "MMMM dd, yyyy") & ". Miłego dnia!"
Debug.Print formattedMessage
```

**Wynik:**
```
Dzisiaj jest 15 kwietnia 2023. Miłego dnia!
```

## Szczegółowe omówienie
Interpolacja ciągów, jaką znamy we współczesnych językach programowania, takich jak Python czy JavaScript, nie istnieje bezpośrednio w VBA. Historycznie, programiści VBA musieli polegać na łączeniu za pomocą `&` lub korzystać z funkcji `Format` do wstawiania wartości do ciągów, co często sprawiało, że proces był uciążliwy dla złożonych ciągów lub wymagał precyzyjnego formatowania. Ta różnica podkreśla epokę pochodzenia VBA i jego skupienie na bezpośredniej prostocie kosztem niektórych współczesnych udogodnień.

Jednakże ważne jest, aby zauważyć, że chociaż VBA nie oferuje wbudowanej interpolacji ciągów, opanowanie `&` dla prostych łączeń lub `Format` dla bardziej złożonych scenariuszy umożliwia mocną i elastyczną manipulację ciągami. Dla programistów pochodzących z języków z natywnymi funkcjami interpolacji ciągów, może to początkowo wydawać się krokiem wstecz, ale te metody oferują poziom kontroli, który po opanowaniu, może być niezwykle potężny. Co więcej, przechodząc do nowszych środowisk .NET, programiści znajdą interpolację ciągów jako funkcję pierwszej klasy w VB.NET, zapewniając bardziej znajome i efektywne podejście do tworzenia dynamicznych ciągów. W praktycznym sensie zrozumienie różnic i ograniczeń w VBA może znacznie pomóc w pisaniu efektywnego, czytelnego kodu i ułatwić przejście do bardziej nowoczesnych środowisk Visual Basic, jeśli będzie to konieczne.
