---
title:    "Clojure: Ein Datum in einen String umwandeln"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Warum 

Das Konvertieren von Datum in eine Zeichenfolge kann für alle Arten von Anwendungen nützlich sein, von der Erstellung von Berichten bis hin zur Speicherung von Daten in einer Datenbank. In diesem Blogbeitrag zeigen wir Ihnen, wie Sie dies in der Programmiersprache Clojure umsetzen können.

# Wie geht man vor

Die Umwandlung eines Datums in eine Zeichenfolge kann in Clojure auf verschiedene Arten erfolgen. Im Folgenden werden wir zwei gängige Methoden vorstellen.

1. Method 1: Verwendung des `format` Befehls

Um ein Datum in einer bestimmten Formatierung umzuwandeln, können Sie den Befehl `format` verwenden. Angenommen, wir haben ein Datum im Format `yyyy-MM-dd` und möchten es in das Format `dd/MM/yyyy` umwandeln. Der Code würde folgendermaßen aussehen:

```Clojure
(let [date (java.util.Date.)]
  (format date "dd/MM/yyyy"))
```

Dies würde als Ausgabe `27/07/2021` erzeugen.

2. Method 2: Verwendung von `SimpleDateFormat`

Eine weitere Möglichkeit ist die Verwendung der `SimpleDateFormat` Klasse. Folgender Code wandelt ein Datum in das Format `dd MMMM yyyy` um:

```Clojure
(let [date (java.util.Date.)
      sdf (java.text.SimpleDateFormat. "dd MMMM yyyy")]
  (.format sdf date))
```

Die Ausgabe wäre in diesem Fall `27 July 2021`.

# Tiefere Einblicke

Beim Arbeiten mit Datum und Zeiten in Clojure ist es wichtig, das `java.time` Paket zu verstehen. Dieses Paket ermöglicht eine einfachere und präzisere Verarbeitung von Datum und Zeit. Zum Beispiel kann die oben genannte Methode 1 mit dem `LocalDate` Objekt aus dem `java.time` Paket wie folgt geschrieben werden:

```Clojure
(require '[java.time :as t])

(let [local-date (t/LocalDate/now)]
  (format local-date "dd/MM/yyyy"))
```

Dies ermöglicht eine flexiblere Handhabung von Datum und Zeit in Clojure.

# Siehe auch

- Offizielle Clojure-Dokumentation zu `format`: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/format
- Offizielle Java-Dokumentation zu `SimpleDateFormat`: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Offizielle Java-Dokumentation zu `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html