---
title:                "Gleam: Wyświetlanie danych debugowania"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś miałby chcieć wyświetlać debugowanie wyjścia? Istnieją wiele powodów, dla których wyświetlanie danych debugowania może być przydatne podczas pisania kodu w języku programowania Gleam. Na przykład, może pomóc zidentyfikować błędy lub dowiedzieć się więcej o działaniu programu.

## Jak to zrobić

Aby wyświetlić debugowanie wyjścia w języku Gleam, użyj funkcji `debug!`. Przykładowy kod wyglądałby tak:

```Gleam
import gleam/debug

let number = 42
debug!(number) // wyświetli 42 w konsoli

let name = "John"
debug!(name) // wyświetli "John" w konsoli
```

Korzystanie z tej funkcji jest proste i pomocne przy sprawdzaniu wartości zmiennych podczas pracy nad kodem.

## Dogłębna analiza

Funkcja `debug!` może również przyjmować więcej niż jedną wartość do wyświetlenia. Na przykład:

```Gleam
import gleam/debug

let age = 24
let name = "Jane"
debug!(name, age) // wyświetli "Jane" i 24 w konsoli
```

Warto również pamiętać, że funkcja ta nie jest dostępna w kodzie produkcyjnym i należy ją usunąć przed udostępnieniem programu na serwerze lub publicznie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o języku programowania Gleam i jego możliwościach, zajrzyj na poniższe linki:

- Oficjalna dokumentacja Gleam: https://gleam.run/
- Repozytorium Gleam na GitHubie: https://github.com/gleam-lang/gleam
- Społeczność Gleam na Discordzie: https://discord.gg/gleam

Mamy nadzieję, że ten artykuł był pomocny i pomoże Ci w dalszej nauce Gleam!