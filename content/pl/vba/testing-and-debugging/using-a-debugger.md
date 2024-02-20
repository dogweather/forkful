---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:30.504801-07:00
description: "U\u017Cycie debugera w Visual Basic dla Aplikacji (VBA) polega na krokowym\
  \ uruchamianiu kodu w celu inspekcji jego przep\u0142ywu wykonania i stan\xF3w zmiennych.\u2026"
lastmod: 2024-02-19 22:04:54.363911
model: gpt-4-0125-preview
summary: "U\u017Cycie debugera w Visual Basic dla Aplikacji (VBA) polega na krokowym\
  \ uruchamianiu kodu w celu inspekcji jego przep\u0142ywu wykonania i stan\xF3w zmiennych.\u2026"
title: Korzystanie z debuggera
---

{{< edit_this_page >}}

## Co i dlaczego?

Użycie debugera w Visual Basic dla Aplikacji (VBA) polega na krokowym uruchamianiu kodu w celu inspekcji jego przepływu wykonania i stanów zmiennych. Proces ten jest kluczowy dla identyfikacji i naprawy błędów w kodzie, co w końcu zapewnia jego oczekiwaną wydajność.

## Jak to zrobić:

W VBA debugger jest integralną częścią Edytora Visual Basic (VBE). Oto jak możesz z niego korzystać:

1. **Ustawianie punktów przerwania**: Kliknij w lewym marginesie obok linii kodu, który Cię interesuje, lub umieść kursor na linii i naciśnij F9. Powoduje to zatrzymanie wykonania VBA w tym miejscu.

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 To 5
            Debug.Print counter ' Ustaw tutaj punkt przerwania
        Next counter
    End Sub
    ```

    Gdy kod zostanie wykonany, zatrzyma się przy linii `Debug.Print counter`, co pozwoli Ci zbadać wartości zmiennych.

2. **Krok Do (F8)**: Za pomocą tego polecenia wykonujesz swój kod instrukcja po instrukcji, wchodząc do ewentualnie wywołanych procedur. Jest to przydatne do śledzenia, jak Twój kod i funkcje wchodzą w interakcje.

3. **Okno Obserwacji (Watch Window)**: Użyj Okna Obserwacji do monitorowania wartości zmiennych lub wyrażeń. Jeśli zmienna nie jest w zasięgu, Okno Obserwacji wskaże to. Kliknij prawym przyciskiem myszy na zmienną > Dodaj Obserwację.

4. **Okno Natychmiastowe (Ctrl+G)**: To okno jest szczególnie użyteczne do testowania wyrażeń lub modyfikowania wartości zmiennych podczas debugowania. Wpisz `?nazwaZmiennej` aby wydrukować aktualną wartość zmiennej, lub przypisz nową wartość z `nazwaZmiennej = nowaWartość`.

    ```vb
    ' W Oknie Natychmiastowym
    ?counter ' Wyświetla aktualną wartość licznika
    counter = 3 ' Ustawia wartość licznika na 3
    ```

5. **Przykładowe Wyjście**:

    Gdy dojdziesz do punktu przerwania i będziesz wykonywać linia po linii za pomocą F8, Okno Natychmiastowe może wyświetlić coś takiego:

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    Tutaj, ręcznie zapytaliśmy o zmienną `counter` po każdej iteracji.

## Pogłębiona analiza:

Debugger w VBA, choć solidny, jest częścią szerszej tradycji narzędzi do debugowania w językach programowania, rozwijając się znacznie od swoich wczesnych poprzedników. Wprowadzony z pierwszymi wersjami VBA, miał na celu zapewnienie programistom prostego, ale potężnego zestawu narzędzi do inspekcji i korekty kodu. Z czasem ulepszenia obejmowały warunkowe punkty przerwania, ulepszone możliwości obserwacji i integrację z interfejsem Excela dla bardziej intuicyjnej inspekcji danych.

Jednak w porównaniu do nowoczesnych Zintegrowanych Środowisk Programistycznych (IDE), takich jak Visual Studio czy Eclipse, narzędzia debugowania VBA mogą wydawać się podstawowe. Te nowoczesne IDE oferują bardziej zaawansowane funkcje, takie jak inspekcja zmiennych w czasie rzeczywistym, zaawansowane punkty przerwania i zintegrowane ramy testowania jednostkowego. Mimo że te alternatywy oferują bardziej kompleksowe doświadczenia z debugowaniem, prostota i bezpośredniość debuggera VBA pozostają dobrze dostosowane do konkretnego kontekstu automatyzacji i tworzenia skryptów w aplikacjach Microsoft Office.

Dla programistów przyzwyczajonych do tych nowoczesnych środowisk, dostosowanie się do narzędzi debugowania VBA może wymagać zmiany podejścia. Jednak zasadnicze zasady inspekcji zmiennych, krokowego przechodzenia przez kod i obserwacji zachowania w czasie wykonania są uniwersalne. Z praktyką, debugger VBA staje się nieodzownym narzędziem do zapewniania, że ​​Twoje skrypty automatyzacji działają bezbłędnie w ekosystemie Office.
