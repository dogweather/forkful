---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:47.655538-07:00
description: "Jak to zrobi\u0107: Pobieranie bie\u017C\u0105cej daty w VBA jest proste,\
  \ za pomoc\u0105 funkcji `Date`, podczas gdy funkcja `Now` dostarcza zar\xF3wno\
  \ bie\u017C\u0105c\u0105 dat\u0119, jak i czas.\u2026"
lastmod: '2024-03-13T22:44:35.243449-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w VBA jest proste, za pomoc\u0105 funkcji\
  \ `Date`, podczas gdy funkcja `Now` dostarcza zar\xF3wno bie\u017C\u0105c\u0105\
  \ dat\u0119, jak i czas."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:
Pobieranie bieżącej daty w VBA jest proste, za pomocą funkcji `Date`, podczas gdy funkcja `Now` dostarcza zarówno bieżącą datę, jak i czas. Oto jak możesz pracować z obiema:

```vb
Sub GetCurrentDate()
    ' Używanie funkcji Date do pobrania bieżącej daty
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Bieżąca data: "; currentDate
    
    ' Używanie funkcji Now do pobrania bieżącej daty i czasu
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Bieżąca data i czas: "; currentDateTime
End Sub
```

Gdy uruchomisz to makro, metoda `Debug.Print` wypisze bieżącą datę oraz bieżącą datę i czas do Okna Natychmiastowego w edytorze VBA. Na przykład:

```
Bieżąca data: 4/12/2023
Bieżąca data i czas: 4/12/2023 15:45:22
```

Pamiętaj, że format daty może się różnić w zależności od ustawień systemowych komputera użytkownika.

## Dogłębna analiza
Funkcje `Date` i `Now` kapsułkują złożoność pracy z datą i czasem w Visual Basic for Applications, zapewniając abstrakcję na poziomie aplikacji, która sprawia, że praca z datami jest prosta i intuicyjna. Historycznie, radzenie sobie z datą i czasem w programowaniu było pełne wyzwań, w tym obsługi różnych stref czasowych, zmian czasu letniego oraz różnych formatów daty.

W VBA, te funkcje opierają się na systemowej dacie i czasie, co oznacza, że są one zależne od lokalizacji użytkownika i ustawień systemowych. To miecz obosieczny, który zapewnia zgodność z otoczeniem użytkownika, ale wymaga także starannego traktowania lokalizacji oraz dostosowań strefy czasowej w globalnych aplikacjach.

Chociaż funkcje daty i czasu VBA są w pełni odpowiednie dla wielu aplikacji, zwłaszcza w zakresie automatyzacji Office, mogą one nie zapewniać precyzji lub granularności wymaganej dla bardziej zaawansowanych aplikacji jak systemy handlu wysokiej częstotliwości czy symulacje naukowe. W takich przypadkach inne środowiska programistyczne lub języki, takie jak Python czy C#, mogą oferować bardziej zaawansowane biblioteki do manipulacji datą i czasem.

Niemniej jednak, dla ogromnej większości zadań związanych z datami i czasami w kontekście aplikacji Excel, Word czy innych aplikacji Office, funkcje `Date` i `Now` w VBA oferują równowagę prostoty, wydajności i łatwości użytkowania, która trudno jest przecenić.
