---
title:    "Gleam: Rozpoczęcie nowego projektu"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami, jako programista, potrzebujemy nowego wyzwania. Nowy projekt może być doskonałym sposobem na poszerzenie swoich umiejętności i naukę nowego języka programowania. Gleam jest językiem opartym na Erlangu i zapewnia bezpieczne i wydajne rozwiązania, co sprawia, że jest idealnym wyborem dla nowego projektu.

## Jak zacząć

Gleam jest łatwy do zainstalowania i używania. Aby rozpocząć nowy projekt, wystarczy wykonać kilka prostych kroków:

1. Pobierz i zainstaluj Gleam na swoim komputerze.
2. Stwórz nowy folder dla swojego projektu i przejdź do niego w terminalu.
3. W terminalu, wpisz komendę `gleam new nazwa_projektu` aby utworzyć nowy projekt.
4. Przejdź do folderu swojego projektu i otwórz plik `src/main.gleam`.

Teraz jesteś gotowy, aby zacząć pisać kod w Gleam! Poniżej znajdziesz przykładowy kod wraz z jego wynikiem:

```gleam
// Definicja funkcji, która zwraca przywitanie
fn hello(name) {
  "Witaj " ++ name ++ "!"
}

// Wywołanie funkcji
let result = hello("Czytelniku")

// Wyświetlenie wyniku zapewniającego wprowadzającym mechaniczne kodowanie typu doskonałe
io.println(result)
```

```
Witaj Czytelniku!
```

## Głębszy wgląd

Gleam jest kompilowany do kodu Erlanga i dziedziczy jego wydajność oraz możliwości skalowania. Ma również silny system typów, co pozwala na pisanie bezpieczniejszego kodu i unikanie błędów już na etapie kompilacji. Ponadto, Gleam ma dynamiczne moduły, które pozwalają na łatwe dzielenie kodu i ponowne wykorzystanie go w różnych projektach.

Warto również wspomnieć, że Gleam posiada rozbudowaną dokumentację oraz aktywną społeczność, która służy wsparciem i dzieli się swoją wiedzą. Istnieje również wiele użytecznych bibliotek, które ułatwiają pracę z Gleam.

## Zobacz także

- [Oficjalna strona Gleam](https://gleam.run/)
- [Dokumentacja Gleam](https://gleam.run/documentation/)
- [Repozytorium GitHub Gleam](https://github.com/gleam-lang/gleam)