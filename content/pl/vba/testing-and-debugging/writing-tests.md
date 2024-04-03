---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:56.230298-07:00
description: "Jak to zrobi\u0107: Cho\u0107 Visual Basic for Applications (VBA) nie\
  \ jest wyposa\u017Cony w wbudowany framework do testowania, podobnie do tych dost\u0119\
  pnych w j\u0119zykach\u2026"
lastmod: '2024-03-13T22:44:35.235914-06:00'
model: gpt-4-0125-preview
summary: "Cho\u0107 Visual Basic for Applications (VBA) nie jest wyposa\u017Cony w\
  \ wbudowany framework do testowania, podobnie do tych dost\u0119pnych w j\u0119\
  zykach takich jak Python czy JavaScript, wci\u0105\u017C mo\u017Cesz implementowa\u0107\
  \ proste procedury testowe do sprawdzania integralno\u015Bci swojego kodu."
title: "Pisanie test\xF3w"
weight: 36
---

## Jak to zrobić:
Choć Visual Basic for Applications (VBA) nie jest wyposażony w wbudowany framework do testowania, podobnie do tych dostępnych w językach takich jak Python czy JavaScript, wciąż możesz implementować proste procedury testowe do sprawdzania integralności swojego kodu. Oto przykład ilustrujący tę ideę:

Załóżmy, że masz w VBA funkcję, która dodaje dwie liczby:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

Aby przetestować tę funkcję, możesz napisać inną procedurę, która weryfikuje jej wyjście względem oczekiwanych wyników:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Test zaliczony!", vbInformation
    Else
        MsgBox "Test niezaliczony. Oczekiwano 15, ale otrzymano " & result, vbCritical
    End If
End Sub
```

Uruchomienie `TestAddNumbers` wyświetli okno wiadomości wskazujące, czy test zakończył się sukcesem, czy porażką, w oparciu o wynik działania funkcji. Choć jest to scenariusz uproszczony, możesz budować bardziej złożone testy, włączając do nich pętle, różne wartości wejściowe, i testowanie wielu funkcji.

## Bardziej szczegółowo
Prezentowana tutaj metoda pisania testów w VBA jest manualna i pozbawiona funkcji bardziej zaawansowanych frameworków do testowania dostępnych w innych środowiskach programistycznych, takich jak automatyczne uruchamianie testów, procedury przygotowania i zakończenia oraz zintegrowane raportowanie wyników testów. Przed szerszym przyjęciem frameworków do testowania jednostkowego i rozwojem sterowanym testami (TDD), manualne procedury testowe, podobne do opisanej, były powszechne. Chociaż ta metoda jest prosta i może być skuteczna dla małych projektów czy celów edukacyjnych, nie jest skalowalna ani efektywna dla większych projektów lub zespołów.

W środowiskach, które wspierają bogatsze zestawy narzędzi deweloperskich, programiści często zwracają się ku frameworkom jak NUnit dla aplikacji .NET czy JUnit dla aplikacji Java, które dostarczają kompleksowych narzędzi do systematycznego pisania i uruchamiania testów. Te frameworki oferują zaawansowane funkcje, takie jak asercje wyników testów, tworzenie obiektów atrap i mierzenie pokrycia kodu.

Dla programistów VBA, szukających bardziej zaawansowanych możliwości testowania, najbliższą alternatywą może być wykorzystanie zewnętrznych narzędzi lub integracja z innymi środowiskami programistycznymi. Niektórzy programiści używają VBA w połączeniu z Excelem, aby ręcznie rejestrować scenariusze testowe i wyniki. Chociaż nie jest to tak wygodne lub zautomatyzowane jak korzystanie z dedykowanego frameworku do testowania, te metody mogą częściowo zapełnić lukę, pomagając utrzymać niezawodność rozwiązań VBA w złożonych lub krytycznych aplikacjach.
