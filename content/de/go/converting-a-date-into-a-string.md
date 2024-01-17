---
title:                "Ein Datum in einen String umwandeln."
html_title:           "Go: Ein Datum in einen String umwandeln."
simple_title:         "Ein Datum in einen String umwandeln."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Konvertieren von Datumsangaben in eine Zeichenfolge ist ein häufiges Problem in der Programmierung. Es beinhaltet die Umwandlung von einem internen Datumsformat in einen lesbaren Text. Dies ist oft erforderlich, um Benutzern ein ansprechendes und verständliches Datum anzuzeigen.

# Wie?

```
Go now := time.Now()
fmt.Println(now.Format("January 2, 2006"))
```
Ausgabe: June 29, 2021

Anstelle eines statischen Formats können auch benutzerdefinierte Formate erstellt werden, um spezifische Datumsangaben anzuzeigen. Zum Beispiel: "02-01-2006" für den 29. Juni 2021.

## Deep Dive

Die Notwendigkeit, Datumsangaben in eine menschenlesbare Form zu bringen, entstand mit der Entwicklung von computergestützten Systemen. Ursprünglich wurden Datumsangaben intern in binären oder hexadezimalen Formaten gespeichert, was für den Benutzer unverständlich war.

Es gibt verschiedene Alternativen zur Konvertierung von Datumsangaben in Strings, wie z.B. die Verwendung von speziellen Datentypen oder das Einbinden von Bibliotheken von Drittanbietern. In Go ist die Verwendung der time-Package die bevorzugte und natürliche Lösung für dieses Problem.

Die Konvertierung von Datum zu String in Go erfolgt durch die Verwendung der Methode "Format" des time-Packages. Das Argument für das gewünschte Format muss dem speziellen Datum vom 2. Januar 2006 entsprechen, das als Referenz im Go-Standard festgelegt ist.

## Siehe Mehr

Weitere Informationen zu den Funktionen und Methoden des time-Packages sowie Beispiele für benutzerdefinierte Formate finden Sie in der offiziellen Dokumentation von Go: https://golang.org/pkg/time/.

Um ein tieferes Verständnis der Hintergründe von Datumsformate in der Programmierung zu erlangen, empfehle ich die Lektüre des Wikipedia-Artikels zu diesem Thema: https://en.wikipedia.org/wiki/Date_format_by_country.