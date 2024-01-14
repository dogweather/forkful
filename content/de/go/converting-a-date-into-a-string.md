---
title:    "Go: Eine Datum in eine Zeichenfolge umwandeln."
keywords: ["Go"]
---

{{< edit_this_page >}}

# Warum

In der Go-Programmierung ist es notwendig, Daten in Strings zu konvertieren, um sie lesbar und verständlicher zu machen. Dies kann auch bei der Arbeit mit Datumsangaben von Vorteil sein.

# Wie funktioniert es?

Um ein Datum in einen String umzuwandeln, kann die Funktion `Format` aus der `time` Bibliothek verwendet werden. Die Syntax hierfür lautet:

```Go
string := time.Format(layout string)
```

Das `layout` Argument ist ein vorgefertigtes Layout oder eine benutzerdefinierte Zeichenfolge, die angibt, wie das Datum angezeigt werden soll. Hier sind einige Beispiele für vorgefertigte Layouts:

```Go
// Standardformat
string := time.Now().Format("Monday, Januar 2, 2006")

// Kurzes Datum
string := time.Now().Format("01/02/2006")

// ISO 8601 Format
string := time.Now().Format("2006-01-02T15:04:05-07:00")
```

Die Funktion `Format` gibt einen String zurück, der das Datum in dem gewünschten Format enthält. Hier ist ein Beispiel für die Ausgabe:

```Go
// Input: 7. Juli 2021
// Layout: Monday, Januar 2, 2006
// Output: Mittwoch, Juli 7, 2021
```

# Tiefen Einblick

In Go gibt es auch die Möglichkeit, benutzerdefinierte Layouts für Datumsangaben zu erstellen. Hierfür kann die `time.Parse` Funktion verwendet werden, die ein benutzerdefiniertes Layout und einen String als Argumente annimmt und ein `time.Time` Objekt zurückgibt.

```Go
customLayout := "2006 Januar MST"
string := "2021 Juli UTC"
time, _ := time.Parse(customLayout, string)
```

Die `Parse` Funktion kann auch verwendet werden, um ein Datum aus einem String zu extrahieren, der bereits ein spezifisches Layout aufweist. Dies ist besonders hilfreich, wenn Daten aus externen Quellen gelesen werden müssen.

```Go
string := "07-07-2021 15:04:05 UTC"
time, _ := time.Parse("01-02-2006 15:04:05 MST", string)
```

# Siehe auch

- [Go Time Package Dokumentation](https://golang.org/pkg/time/)
- [Zeitformate in Go](https://yourbasic.org/golang/format-parse-string-time-date-example/)
- [Tiefere Einblicke in das Arbeiten mit Zeit in Go](https://www.calhoun.io/working-with-dates-and-times-in-go/)