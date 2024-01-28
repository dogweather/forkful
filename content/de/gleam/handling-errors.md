---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:51:59.093776-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung bedeutet, dass man in seinem Code damit rechnet, dass Dinge schiefgehen können, und dass man diese Situationen auf elegante Weise bewältigt. Programmierer tun dies, weil es Anwendungen robust und benutzerfreundlich hält, selbst wenn Unerwartetes passiert.

## Wie man es macht:
In Gleam verwendet man oft den `Result`-Typ für die Fehlerbehandlung. Es ist ein Enum mit zwei Varianten: `Ok` (für Erfolg) und `Error` (für Misserfolg). Hier ist ein einfaches Beispiel:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Hoppla! Es ist kaputtgegangen.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Wenn Sie `main` mit `might_fail(False)` ausführen, gibt es `42` zurück. Wenn Sie `True` übergeben, wird "Hoppla! Es ist kaputtgegangen." ausgegeben und `0` zurückgegeben.

## Vertiefung
Gleams Ansatz zur Fehlerbehandlung ist von seinen Erlang-Wurzeln beeinflusst. Historisch verwendet Erlang eine Philosophie des „Lass es abstürzen“, und verlässt sich auf Überwachungsbaumstrukturen, um Prozessfehler zu verwalten. Wenn Sie jedoch Gleam-Code schreiben, der sich nicht innerhalb eines Prozesses befindet, der überwacht werden soll, wie etwa innerhalb einer Bibliotheksfunktion, möchten Sie Fehler ausdrücklich behandeln.

Alternativen zur Verwendung von `Result` umfassen den `Option`-Typ für Fälle, in denen etwas `None` (nichts) oder `Some` (etwas) sein könnte, die jedoch keine Fehlerinformation tragen. Um Fehler über Prozessgrenzen hinweg zu signalisieren, könnte man Erlangs Nachrichtenaustauschmechanismen verwenden.

Gleams Fehlerbehandlung spiegelt einen funktionalen Programmierstil wider, bei dem Nebenwirkungen (wie Fehler) mit Typen und Musterabgleichung verwaltet werden, was Klarheit und Vorhersehbarkeit im Fehlermanagement bietet.

## Siehe auch
- [Fehlerbehandlung in Erlang](http://erlang.org/doc/reference_manual/errors.html)
