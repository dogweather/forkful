---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Gleam: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Szenarien, in denen es erforderlich ist, ein Datum in einen String umzuwandeln. Zum Beispiel kann es nützlich sein, wenn man Datumswerte in einem bestimmten Format anzeigen möchte oder wenn man Datumswerte in einem Text verwenden muss. In Gleam ist dies besonders einfach und effektiv, daher ist es wichtig zu verstehen, wie man ein Datum in einen String umwandeln kann.

## Wie geht's

Der Prozess, ein Datum in einen String umzuwandeln, ist in Gleam sehr simpel. Alles, was du tun musst, ist die `date_to_string`-Funktion zu verwenden und das gewünschte Format als Argument anzugeben. Hier ist ein Beispiel:

```Gleam
let meinDatum = Date.fromString("2021-01-01")
let meinDatumString = Date.to_string(meinDatum, "dd.mm.yyyy")
```

Im obigen Code werden wir zuerst ein `Date`-Objekt mit dem Wert "2021-01-01" erstellen und dann die `date_to_string`-Funktion verwenden, um es in einen String mit dem Format "dd.mm.yyyy" umzuwandeln. Das Ergebnis wird in der Variablen `meinDatumString` gespeichert.

Wenn du das Gleam-Modul `Date` importiert hast, stehen dir verschiedene Formatierungsoptionen zur Verfügung, einschließlich `dd.mm.yyyy`, `dd/mm/yyyy` und `mm/dd/yyyy`. Du kannst auch benutzerdefinierte Formate erstellen, indem du die entsprechenden Platzhalter wie `dd`, `mm` und `yyyy` verwendest. Hier ist ein weiteres Beispiel mit einem benutzerdefinierten Format:

```Gleam
let meinDatum = Date.fromString("2021-01-01")
let meinDatumString = Date.to_string(meinDatum, "yyyy-mm-dd")
```

Das Ergebnis dieses Codes wird "2021-01-01" sein.

## Tiefentauchen

Obwohl das Konvertieren von Datumswerten in Strings in Gleam sehr einfach ist, gibt es einige Dinge, die du beachten solltest. Zum Beispiel kann die Eingabe eines ungültigen Formates zu einem Fehler führen. Daher ist es wichtig, sicherzustellen, dass das angegebene Format korrekt ist.

Außerdem funktioniert die `date_to_string`-Funktion nur mit `Date`-Objekten, die mit der `fromString`-Funktion erstellt wurden. Wenn du also bereits ein `Date`-Objekt hast, das aus anderen Methoden erstellt wurde, musst du es zuerst mit `fromString` konvertieren, bevor du es in einen String umwandeln kannst.

## Siehe Auch

- Die offizielle Gleam Dokumentation zum Thema Datumswerte: https://gleam.run/books/standard-library/functions#date
- Ein einfaches Tutorial zum Arbeiten mit Datumswerten in Gleam: https://www.erlang-solutions.com/blog/convert-dates-into-strings-in-gleam.html