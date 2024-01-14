---
title:    "Fish Shell: Umwandeln eines Datums in eine Zeichenkette"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Konvertieren von einem Date in eine Zeichenkette ist eine nützliche Fähigkeit, die in der Programmierung häufig benötigt wird. Es ermöglicht uns, Daten in einem für uns verständlichen Format anzuzeigen und zu manipulieren.

# Wie

Um ein Datum in eine Zeichenfolge zu konvertieren, können wir die in Fish Shell eingebaute Funktion `date` verwenden. Diese Funktion akzeptiert ein Datum im Format `YYYY-MM-DD` und gibt es als Zeichenfolge aus.

```
Fish Shell Datumskonvertierung Beispiel:

$ date 2021-09-01
1. September 2021

$ date -f "%d.%m.%Y" 2021-09-01
01.09.2021
```

Die `-f` Option erlaubt es uns, das Ausgabeformat nach unseren eigenen Wünschen anzupassen. Die vollständige Liste der möglichen Formatierungsoptionen ist in der Fish Shell Dokumentation zu finden.

# Deep Dive

Die `date` Funktion verwendet die in Ihrem System eingestellte Spracheinstellung, um die Ausgabe zu formatieren. Wenn Sie also möchten, dass die Ausgabe in einer anderen Sprache erscheint, können Sie dies über die Umgebungsvariable `LANG` angeben.

Ein Beispiel für das Konvertieren von einem Datum in eine Zeichenfolge in Deutsch:

```
$ set -x LANG "de_DE.UTF-8"
$ date 2021-09-01
1. September 2021
```

Es ist auch möglich, eine andere Zeichenfolge anstelle eines Datums zu übergeben und sie als Datum zu interpretieren. Dies kann nützlich sein, wenn Sie beispielsweise eine Zeichenkette aus einer Benutzereingabe erhalten und sie als Datum verwenden möchten.

```
$ date -f "%Y/%m/%d" 2021/09/01
01. September 2021
```

Diese Funktionen sind nur einige Beispiele für das Konvertieren von einem Datum in eine Zeichenfolge in Fish Shell. Es gibt noch viele weitere Optionen, die Sie in der Dokumentation entdecken können.

# Siehe auch

- Offizielle Fish Shell Dokumentation: https://fishshell.com/docs/current/cmds/date.html
- Umgebungsvariable `LANG` in Fish Shell: https://fishshell.com/docs/current/variables.html#locale.lang