---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów wiersza poleceń polega na wejściu w dania dostarczone dla twojego programu poprzez terminal. Programiści robią to, aby umożliwić użytkownikom spersonalizowanie działania programu.

## Jak to robić:

W Elm (aktualna wersja), nie ma natywnej obsługi dla czytania argumentów z linii poleceń, ale możemy to osiągnąć za pomocą JavaScript.

```Elm
port module Main exposing (..)

port toJS : String -> Cmd msg

main =
    toJS "Hello JavaScript!"
```

Gdzie "toJS" to port, który wysyła komunikaty z Elm do JavaScript.

W pliku html, który uruchamia kod Elm:

```JavaScript
<script>
var app = Elm.Main.init();

app.ports.toJS.subscribe(function(data) {
  console.log(data);  // Prints: "Hello JavaScript!"
});
</script>
```

## Deep Dive

Elm, będący językiem osadzonym w środowisku przeglądarki, nie ma bezpośredniej możliwości interakcji z wierszem poleceń. Dlatego, aby przekazać argumenty z linii poleceń do Elm, będziesz musiał wykorzystać JavaScript.

Alternatywą jest wykorzystanie języka, który ma natywną obsługę takich argumentów, na przykład Node.js, a następnie komunikować się z Elm za pomocą portów.

Szczegółowa implementacja może zależeć od specyfiki twojego projektu, ale generalnie polegać będzie na wczytaniu argumentów w JavaScript, a następnie przesłaniu ich do Elm.

## Zobacz też

- [Elm Guide - Interoperability](https://guide.elm-lang.org/interop/)