---
title:                "Odczytywanie argumentów z wiersza poleceń"
html_title:           "Gleam: Odczytywanie argumentów z wiersza poleceń"
simple_title:         "Odczytywanie argumentów z wiersza poleceń"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś chciałby czytać argumenty wiersza poleceń? Jeśli pracujesz z programami wiersza poleceń lub piszesz skrypty, z pewnością masz już doświadczenie z wczytywaniem argumentów. Jeśli jednak dopiero zaczynasz używać języka programowania Gleam, może Ci się przydać wiedza na temat tej ważnej funkcji.

## Jak to zrobić

Gleam oferuje prosty sposób na wczytywanie argumentów wiersza poleceń. Możesz to zrobić za pomocą funkcji ```Gleam.IO.Terminal.args()```, która zwraca listę argumentów podanych przez użytkownika. Przyjrzyjmy się przykładowemu kodowi:

```Gleam
import Gleam.IO.Terminal as Terminal

args = Terminal.args()

```

W tym przykładzie, ```args``` będzie zawierać listę argumentów podanych przez użytkownika wiersza poleceń.

Teraz sprawdźmy, jak to działa w praktyce. Załóżmy, że jako argumenty podano nazwy dwóch plików:

```Gleam
import Gleam.IO.Terminal as Terminal

args = Terminal.args()

case args {
  [] -> Terminal.print("Brak argumentów")
  [file1, file2] -> Terminal.print("Otrzymane pliki: " ++ file1 ++ " i " ++ file2)
  _ -> Terminal.print("Podano za dużo argumentów")
}
```

W tym przykładzie wykorzystujemy wzorzec w przypadku, gdy użytkownik podał dwa argumenty. Jeśli więc uruchomisz ten kod, a następnie wierszu poleceń wpiszesz: ```Gleam run app.gleam file1.txt file2.txt```, to otrzymasz wiadomość: ```Otrzymane pliki: file1.txt i file2.txt```.

## Zanurzenie w temat

Funkcja ```Gleam.IO.Terminal.args()``` zwraca listę typu ```List(String)```, co oznacza, że możesz wykorzystać wszelkie funkcje dostępne dla list w języku Gleam. Na przykład, możesz wykorzystać funkcję ```List.map()```:

```Gleam
import Gleam.IO.Terminal as Terminal
import Gleam.List as List

args = Terminal.args()

upper_args = List.map(String.to_upper, args)

Terminal.print(upper_args)
```

Po uruchomieniu otrzymasz listę argumentów przekonwertowanych na duże litery. Możesz też wykorzystać funkcję ```List.filter()```, aby pozbyć się pewnych argumentów, lub ```List.find()```, aby znaleźć konkretny argument na liście.

## Zobacz także

Sprawdź dokładną dokumentację dla funkcji ```Gleam.IO.Terminal.args()``` oraz dowiedz się więcej o innych możliwościach jakie daje Gleam w dziedzinie operacji na argumentach wiersza poleceń. Oto kilka przydatnych linków:

- [Dokumentacja Gleam](https://gleam.run/)
- [Funkcje dostępne dla list w Gleam](https://gleam.run/modules/list.html)
- [Przykłady zastosowania argumentów wiersza poleceń w Gleam](https://github.com/search?q=language%3Agleam+command+line+arguments&type=Code)