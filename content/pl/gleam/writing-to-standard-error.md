---
title:    "Gleam: Pisanie do standardowego błędu"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania kodu w Gleam, zamiast standardowego wyjścia, chcemy wyświetlić pewne informacje na standardowe wyjście błędu. Może to być przydatne w celu debugowania lub raportowania błędów w naszym programie. W tym wpisie pokażę Ci, jak to zrobić.

## Jak to zrobić

Aby wypisać dane na standardowe wyjście błędu, możemy użyć funkcji `stderr.write` z modułu `gleam/core/io`.

``` Gleam
import gleam/core/io

gleam/core/io.stderr.write("Błąd: Nie znaleziono pliku!")
```

Ten kod spowoduje wypisanie tekstu "Błąd: Nie znaleziono pliku!" na standardowe wyjście błędu. Możemy również użyć tej funkcji do formatowania danych, tak jak w przypadku funkcji `stdout` wyświetlającej dane na standardowym wyjściu.

## Deep Dive

Funkcja `stderr.write` przyjmuje jako argument dowolną wartość i zwraca `Result(Int, String)`, czyli wynik zawierający informację o sukcesie lub błędzie. Jeśli wynik jest sukcesem, zwrócony zostanie `Ok(())`. W przypadku błędu, zostanie zwrócona wartość `Err`, zawierająca numer błędu oraz komunikat.

Możemy również skorzystać ze specjalnej funkcji `stderr.write_err` do wypisywania błędów na standardowe wyjście błędu. Ta funkcja przyjmuje jako pierwszy argument numer błędu, a jako drugi dowolną wartość. Przykład użycia:

``` Gleam
import gleam/core/io

gleam/core/io.stderr.write_err(404, "Nie znaleziono pliku")
```

## See Also

- Dokumentacja modułu gleam/core/io: https://gleam.run/modules/gleam/core/io.html
- Przykładowe projekty w Gleam: https://github.com/gleam-lang/awesome-gleam

Dzięki użyciu funkcji `stderr.write` i `stderr.write_err` możemy łatwo wypisywać informacje na standardowe wyjście błędu w naszych programach. Mam nadzieję, że ten wpis był dla Ciebie pomocny. Nie zapomnij również sprawdzić innych modułów i przykładowych projektów w Gleamie. Happy coding! 🚀