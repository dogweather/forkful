---
title:                "Gleam: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele programów musi być w stanie odczytać argumenty wprowadzone przez użytkownika w wierszu poleceń. W tym poście dowiesz się, jak czytać argumenty wiersza poleceń w Gleam.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w Gleam, musisz najpierw zaimportować moduł `Cmdline`:

```Gleam
import gleam/cmdline

```

Następnie, możesz użyć funkcji `Cmdline.args()` aby odczytać wszystkie argumenty wiersza poleceń jako listę napisów:

```Gleam
fn main() {
  args = Cmdline.args()

  // args to lista napisów
  // możesz teraz wykonać na niej operacje
  // np. wypisać ich zawartość
  for arg in args {
    Cmdline.println(arg)
  }
}

```
Przykładowe wyjście:
```
$ gleam run program.gleam argument1 argument2 argument3
argument1
argument2
argument3
```

## Deep Dive

Za każdym razem, gdy uruchamiasz program w wierszu poleceń, wszystkie wprowadzone argumenty są przekształcane w listę argumentów i przekazywane do funkcji `main`. Możesz wykorzystać tę umiejętność do przekazywania różnych opcji do swojego programu w celu wpływania na jego działanie. Możesz również dostosowywać zachowanie swojego programu w zależności od liczby i kolejności argumentów.

## Zobacz również

- Dokumentacja modułu `Cmdline`: https://gleam.run/modules/gleam_cmdline/latest/Cmdline.html
- Przykładowy program odczytujący argumenty wiersza poleceń: https://github.com/gleam-lang/gleam/blob/master/examples/cli/program.gleam