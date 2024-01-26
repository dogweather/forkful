---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben ist das Komponieren von Code, der anderen Code überprüft. Programmierer machen das, um Fehler zu vermeiden, Sicherheit zu erhöhen und den Code leichter wartbar zu machen.

## How to:
```Gleam
import gleam/should
import your_module

pub fn your_test_function() {
  your_module.your_function_to_test()
  |> should.equal("Erwartetes Ergebnis")
}
```

Ausgabe nach Testlauf:
```
test your_test_function ... ok
```

## Deep Dive
Gleam baut auf der Erlang VM auf, einer Plattform bekannt für Robustheit und Langlebigkeit in systemkritischen Anwendungen. Traditionelle Alternativen wären Eunit oder Common Test in Erlang, doch Gleam bietet eine moderne, typsichere Option. Die Integration des `should`-Moduls vereinfacht die Syntax und sorgt für lesbare Assertions. Im Kern läuft alles über das 'gleam_otp' Framework, das Parallelität und Fehlermanagement von Erlang nutzt.

## See Also
- Eine Sammlung an Gleam-Modulen: [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam)
- Das 'gleam_otp' Framework: [gleam_otp](https://hex.pm/packages/gleam_otp)
