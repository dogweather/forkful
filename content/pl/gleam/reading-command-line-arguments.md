---
title:    "Gleam: Odczytywanie argumentów wiersza poleceń"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Podczas pisania programów, często potrzebujemy, aby program przyjmował pewne argumenty z linii poleceń. To pozwala nam na dostosowanie działania programu bez konieczności zmieniania kodu źródłowego. W tym artykule dowiecie się, jak odczytywać argumenty z linii poleceń w języku Gleam.

## Jak To Zrobić

Aby odczytać argumenty z linii poleceń w Gleam, należy użyć funkcji `gleam/io/Cli.arguments` z modułu "gleam/io". Poniżej znajduje się przykładowy kod, który wypisze na ekranie podane argumenty:
```Gleam
external println(message: String) 
  -> Ok

let main = _ ->
  let args = 
    case io/Cli.arguments() in
      Ok(cli_args) -> cli_args
      Err(_) -> [""]
  for arg in args do
    println(arg)
  Ok(())
```

Gdy uruchomimy ten program z argumentami `"hello"` i `"world"`, powinniśmy zobaczyć następujący output:
```
hello
world
```

## Głębsza Analiza

Funkcja `cli.arguments` zwraca wynik typu `Result`, który może być albo `Ok` (zawierającym tablicę argumentów) lub `Err`. Warto zauważyć, że funkcja ta zwróci błąd tylko wtedy, gdy program jest uruchomiony bez podania żadnych argumentów z linii poleceń. W przeciwnym razie zwraca ona pustą tablicę.

Dodatkowym ulepszeniem powyższego kodu może być sprawdzenie ilości argumentów i wyświetlenie odpowiedniego komunikatu w celu obsłużenia błędów.

## Zobacz także

- Dokumentacja dla modułu `gleam/io`: https://gleam.run/docs/io
- Dokumentacja dla `Result` w języku Gleam: https://gleam.run/docs/result
- Przykładowe projektu wykorzystujące odczytywanie argumentów z linii poleceń: https://github.com/gleam-lang/examples/blob/master/cli_arguments/src/main.gleam