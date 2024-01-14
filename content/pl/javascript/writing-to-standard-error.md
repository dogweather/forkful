---
title:    "Javascript: Pisanie do standardowego błędu"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać do standardowego błędu w języku Javascript

Pisanie do standardowego błędu jest kluczowym elementem programowania w języku Javascript. Jest to przydatne narzędzie, które pozwala programistom wyświetlać błędy i ostrzeżenia w konsoli, co ułatwia debugowanie i poprawę kodu. W tym artykule dowiesz się, dlaczego warto pisać do standardowego błędu i jak to zrobić.

## Jak pisać do standardowego błędu w języku Javascript

W języku Javascript istnieją dwa sposoby na wypisywanie błędów do standardowego błędu - `console.error()` i `process.stderr.write()`. Pierwsza metoda jest częściej używana i jest dostępna we wszystkich przeglądarkach. Druga metoda jest dostępna tylko w środowisku Node.js. Poniżej przedstawiono przykładowy kod, który wykorzystuje obie metody.

```Javascript
// Wypisywanie błędu przy użyciu console.error()
console.error("To jest jakiś błąd!");
// Wypisywanie błędu przy użyciu process.stderr.write()
process.stderr.write("To jest kolejny błąd!");
```

Przy użyciu `console.error()` możemy również przekazać wiele argumentów, a następnie wyświetlić je w standardowym błędzie. Na przykład:

```Javascript
let liczba = 5;
console.error("Wartość liczby to", liczba);
```

Wyjście z powyższego kodu będzie wyglądać następująco:

```
Wartość liczby to 5
```

## Pogłębione zagadnienia dotyczące pisania do standardowego błędu

Pisanie do standardowego błędu może być bardzo przydatne w przypadku, gdy chcemy wyświetlić błąd lub ostrzeżenie w odpowiednim miejscu kodu. Może również pomóc nam zlokalizować problem i szybciej go naprawić. Poniżej przedstawiono przykłady użycia pisanie do standardowego błędu w różnych sytuacjach.

### Wyświetlanie błędu w przypadku błędnego argumentu funkcji

Jedną z częstych sytuacji, w których możemy wykorzystać pisanie do standardowego błędu, jest sprawdzenie poprawności argumentów funkcji. Możemy dodać warunek, który sprawdzi, czy argument jest prawidłowy, a jeśli nie, to wyświetli błąd przy użyciu `console.error()`.

```Javascript
function dodaj(a, b) {
    // Sprawdzamy, czy argumenty są liczbami
    if (typeof a !== "number" || typeof b !== "number") {
        // Wyświetlamy błąd, jeśli argumenty nie są liczbami
        console.error("Oba argumenty muszą być liczbami!");
        return;
    }
    // Wykonujemy operację dodawania
    return a + b;
}

// Wywołujemy funkcję z błędnym argumentem
dodaj(2, "abc");
```

Wyjście w konsoli będzie wyglądać następująco:

```
Oba argumenty muszą być liczbami!
```

### Wyświetlanie błędu w przypadku nieobsłużonego wyjątku

Wyjątki mogą być trudne do zlokalizowania, szczególnie w większym i bardziej złożonym kodzie. W takiej sytuacji przydatne jest wyświetlenie informacji o wyjątku w standardowym błędzie, abyśmy mogli go łatwiej znaleźć i naprawić.

```Javascript
try {
    // Wywołanie funkcji, która nie istnieje
    nieznanaFunkcja();
} catch (err) {
    // Wyświetlenie błę