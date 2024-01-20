---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów linii poleceń to proces, w którym program interpretuje dane wprowadzane do niego za pomocą interfejsu wiersza poleceń. Programiści robią to, aby umożliwić użytkownikom modyfikowanie działania programu na podstawie podanych argumentów.

## Jak to zrobić:

Przykład czytania argumentów linii poleceń w języku Lua.

Parametry przekazane do programu Lua są dostępne przez globalną tablicę o nazwie `arg`. 

Skrypt Lua:

```Lua
for i = 0, #arg do
  print("Argument", i, "=", arg[i])
end
```

Przykładowy wynik dla polecenia `lua script.lua test1 test2`:

```Lua
Argument 0 = script.lua
Argument 1 = test1
Argument 2 = test2
```

## Pogłębiona analiza

Początkowo, w Lua 5.0, argumenty linii poleceń nie były dostępne bezpośrednio - można było je uzyskać tylko za pomocą `arg`. Dopiero w Lua 5.1 wprowadzono globalną tablicę `arg`.

Jeżeli chodzi o alternatywy, są inne języki programowania, które obsługują argumenty linii poleceń, takie jak Python, C++ i Java. W tych językach stosuje się różne podejścia, od prostych tablic (jak w C++) do bardziej zaawansowanych narzędzi, takich jak parsowanie argumentów w Pythonie.

Jeżeli chodzi o szczegóły implementacji, tablica `arg` zawiera nie tylko argumenty linii poleceń, ale także pewne dodatkowe wartości. `arg[-1]` to interpreter Lua, `arg[0]` to nazwa skryptu, a `arg[n]` to reszta argumentów.

## Zobacz także

Poniżej jest kilka linków do dodatkowych zasobów dotyczących czytania argumentów linii poleceń:

1. [Dokumentacja Lua gdzie opisana jest tablica `arg`](http://www.lua.org/manual/5.4/manual.html#pdf-arg)
3. [Poradnik jak przerabiać argumenty linii poleceń w Pythonie](https://docs.python.org/3/howto/argparse.html)