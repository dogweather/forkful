---
title:    "Gleam: Odczytywanie argumentów linii poleceń"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których powinieneś nauczyć się czytać argumenty z wiersza poleceń w Gleam. Przede wszystkim, jest to bardzo przydatna umiejętność, szczególnie jeśli pracujesz z większymi projektami. Pozwala to na dostosowanie zachowania programu w zależności od tego, w jaki sposób jest uruchamiany.

## Jak to zrobić

Aby wczytać argumenty z wiersza poleceń w Gleam, należy użyć funkcji `os.args()`, która zwraca listę argumentów przekazanych przy uruchamianiu programu. Następnie można przetworzyć tę listę w celu uzyskania wartości, które są potrzebne w programie.

Przykładowy kod:

```Gleam
import os

pubfn main() {
  args = os.args()
  for arg in args {
    // przetwarzanie argumentu
  }
}
```

Przykładowy output dla komendy `gleam run program "argument1" 2`:

```Bash
["program", "argument1", "2"]
```

## Vert Deep Dive

Funkcja `os.args()` zwraca listę argumentów jako typ `List(String)`. Taki typ może być przetwarzany na inne typy, na przykład `Int` lub `Float`, zależnie od potrzeb programu.

Ważne jest również zauważyć, że argumenty z wiersza poleceń są przekazywane jako ciągi znaków. W przypadku, gdy wymagane są inne typy, należy odpowiednio je przekonwertować.

## Zobacz także

- [Dokumentacja Gleam na temat argumentów z wiersza poleceń](https://gleam.run/book/tour/command-line-arguments.html)
- [Przykładowy projekt z wykorzystaniem argumentów z wiersza poleceń w Gleam](https://github.com/yourusername/your-project-name)