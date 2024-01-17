---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "Gleam: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Was ist "writing to standard error" und warum machen Programmierer das?

"Writing to standard error" bedeutet, dass man spezifische Informationen oder Fehlermeldungen ausgibt, die vom Computer auf der Konsole angezeigt werden. Programmierer machen das, um während der Entwicklung und beim Testen ihres Codes schnell und effizient Fehler zu finden und zu beheben.

Wie geht das?
In Gleam kann man standard error mit der Funktion `log_error` aufrufen, die Teil des `gleam/standard_library` Moduls ist. Hier sind einige Beispiele für die Verwendung von `log_error` und die dazugehörigen Output-Nachrichten:

```Gleam
log_error("Oops! Something went wrong!")
// Output: Oops! Something went wrong!

let name = "John"
log_error("Hello, " ++ name)
// Output: Hello, John
```

Tiefergehende Informationen
Das Schreiben von Meldungen auf standard error ist eine häufige Praxis in der Softwareentwicklung. Es ermöglicht Programmierern, wichtige Informationen zu verfolgen und zu debuggen, während ihr Code ausgeführt wird. Früher war es oft erforderlich, dass Programmierer manuell printf-Anweisungen in ihrem Code hinzufügen, um Informationen auf der Konsole auszugeben. Mit standard error und der `log_error` Funktion ist das jedoch viel einfacher und effizienter. Alternativ können Programmierer auch standard output verwenden, um Informationen auszugeben, wenn sie nicht sofort wichtig sind.

Siehe auch
Weitere Informationen und Beispiele zur Verwendung von standard error und `log_error` finden Sie in der offiziellen Gleam-Dokumentation unter https://gleam.run/books/standard-library#logging sowie in der Gleam Github-Repository unter https://github.com/gleam-lang/gleam.