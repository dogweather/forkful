---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Standardfehler, auch bekannt als `stderr`, ist ein Ausgabekanal, der für die Ausgabe von Fehlermeldungen und Diagnoseinformationen verwendet wird. Programmierer nutzen ihn, um Fehlermeldungen von normalen Ausgabedaten zu trennen, was das Debuggen erleichtert und die Integration in andere Softwaretools verbessert.

## Wie geht das:
JavaScript bietet über `console.error` eine einfache Methode, um auf `stderr` zu schreiben. Hier ist ein Beispiel:

```Javascript
console.error('Das ist eine Fehlermeldung.');
```

Die Ausgabe sieht dann so aus:
```
Das ist eine Fehlermeldung.
```

## Tiefgang:
Historisch gesehen entstand die Praxis, Standardfehler zu verwenden, in den Unix-Systemen, um Fehler besser handhaben zu können. Es gibt auch Alternativen, wie das Schreiben in Log-Dateien, aber `stderr` ist standardmäßig in den meisten Programmierumgebungen verfügbar und benötigt keine zusätzliche Konfiguration. In Node.js zum Beispiel wird `stderr` mit `process.stderr.write()` direkt beschrieben:

```Javascript
process.stderr.write("Detailierter Fehleroutput.\n");
```

Diese niedrigere Ebene der Ausgabe kann für komplexere Logging-Mechanismen oder zur Kombination mit anderen Streams nützlich sein.

## Siehe Auch:
- MDN Web Docs zu `console.error`: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Node.js Dokumentation zu `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Artikel über die Unterschiede zwischen stdout und stderr auf Unix-Systemen: https://www.jstor.org/stable/unixsystems.12