---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:45.753915-07:00
description: "Tablice asocjacyjne, cz\u0119sto nazywane s\u0142ownikami w Visual Basic\
  \ dla Aplikacji (VBA), pozwalaj\u0105 programistom na tworzenie kolekcji par klucz-warto\u015B\
  \u0107. Ta\u2026"
lastmod: '2024-03-13T22:44:35.223745-06:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne, cz\u0119sto nazywane s\u0142ownikami w Visual Basic\
  \ dla Aplikacji (VBA), pozwalaj\u0105 programistom na tworzenie kolekcji par klucz-warto\u015B\
  \u0107. Ta\u2026"
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Co i dlaczego?

Tablice asocjacyjne, często nazywane słownikami w Visual Basic dla Aplikacji (VBA), pozwalają programistom na tworzenie kolekcji par klucz-wartość. Ta funkcja jest kluczowa dla efektywnego przechowywania i odzyskiwania danych, oferując bardziej elastyczny i intuicyjny sposób zarządzania danymi niż tradycyjne indeksy tablic.

## Jak używać:

W VBA obiekt `Dictionary` zapewnia funkcjonalność podobną do tablic asocjacyjnych. Musisz najpierw dodać odniesienie do Microsoft Scripting Runtime, aby go użyć:

1. W edytorze VBA przejdź do Narzędzia > Odniesienia...
2. Zaznacz "Microsoft Scripting Runtime" i kliknij OK.

Oto jak zadeklarować, wypełnić i uzyskać dostęp do elementów w `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Dodawanie elementów
sampleDictionary.Add Key:="Name", Item:="Jan Kowalski"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Inżynier"

' Dostęp do elementów
Debug.Print sampleDictionary.Item("Name")  ' Wynik: Jan Kowalski
Debug.Print sampleDictionary.Item("Age")   ' Wynik: 29

' Sprawdzanie czy klucz istnieje
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Klucz Occupation istnieje"
End If

' Usuwanie elementów
sampleDictionary.Remove("Occupation")

' Przechodzenie przez słownik
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Głębsze spojrzenie

Obiekt `Dictionary` pod spodem łączy się ze składnikami Windows Scripting Host. Jako taki, jest późnozwiązanym obiektem COM, który był w przeszłości powszechnym sposobem rozszerzania funkcjonalności VBA. Jego użycie w VBA może znacząco zwiększyć zdolność języka do manipulowania złożonymi zestawami danych bez narzucania sztywnej struktury, jak w przypadku tradycyjnych tablic lub zakresów Excela.

Jednym z ograniczeń do zapamiętania jest to, że dostęp do `Dictionary` wymaga ustawienia odniesienia do Microsoft Scripting Runtime, co może komplikować dystrybucję projektów VBA. Alternatywy takie jak kolekcje istnieją wewnątrz VBA, ale brakuje im niektórych kluczowych cech `Dictionary`, takich jak łatwość sprawdzania istnienia klucza bez wywoływania błędu.

W bardziej nowoczesnych kontekstach programistycznych języki takie jak Python oferują wbudowane wsparcie dla tablic asocjacyjnych (znanych również jako słowniki w Pythonie) bez potrzeby dodawania zewnętrznych odniesień. To wbudowane wsparcie usprawnia proces i oferuje bardziej zaawansowane funkcje "od ręki". Jednak w ramach VBA i dla specyficznych zastosowań związanych z automatyzacją zadań w pakiecie Microsoft Office, użycie obiektu `Dictionary` pozostaje potężną i stosowną metodą na struktury danych podobne do tablic asocjacyjnych.
