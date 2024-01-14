---
title:    "Go: Das aktuelle Datum erhalten"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum und die aktuelle Zeit sind wichtige Informationen, die in vielen Programmen benötigt werden. In diesem Blogbeitrag werden wir uns mit der Frage beschäftigen, wie man in Go das aktuelle Datum und die aktuelle Uhrzeit abrufen kann.

## Wie geht das?

Um das aktuelle Datum zu erhalten, können wir in Go die Funktion `time.Now()` verwenden. In unserem Beispiel möchten wir das Datum anzeigen, also müssen wir die `Date()` Methode aufrufen.

```Go
currentDate := time.Now().Date()
```

Jetzt haben wir das aktuelle Datum in einer Variablen `currentDate` gespeichert. Um es anzuzeigen, können wir entweder die `fmt.Println()` Funktion verwenden oder wir können das Datum direkt in einem String formatieren.

```Go
// Ausgabe über fmt.Println()
fmt.Println(currentDate)

// Ausgabe über einen formatierten String
fmt.Printf("%d.%02d.%02d", currentDate.Day(), currentDate.Month(), currentDate.Year())
```

Die Ausgabe wird je nach aktuellem Datum unterschiedlich sein, z.B.`2021-08-25` oder `25.08.2021`. Wenn wir auch die Uhrzeit anzeigen möchten, können wir die `Time()` Methode anstatt der `Date()` Methode aufrufen.

```Go
currentDateTime := time.Now()

// Ausgabe über fmt.Println()
fmt.Println(currentDateTime)

// Ausgabe über einen formatierten String
fmt.Printf("%02d:%02d:%02d", currentDateTime.Hour(), currentDateTime.Minute(), currentDateTime.Second())
```

Die Ausgabe wird ähnlich aussehen wie `2021-08-25 13:45:30` oder `13:45:30`.

## Tiefergehende Infos

Die Funktion `time.Now()` gibt uns einen `time.Time` Datentyp zurück, der nicht nur das Datum und die Uhrzeit, sondern auch Informationen über Zeitzonen und unterstützte Operationen beinhaltet. Wir können auf diese Informationen zugreifen, indem wir Methoden wie `Location()` oder `Add()` auf den `time.Time` Datentyp anwenden. Beispielsweise können wir die aktuelle Zeit um eine Stunde erhöhen, indem wir `time.Now().Add(time.Hour)` verwenden.

Es ist auch möglich, das aktuelle Datum und die aktuelle Zeit in einer bestimmten Zone abzurufen, indem wir die `time.LoadLocation()` Funktion verwenden. Diese Funktion erwartet eine Zeitzonenabkürzung oder einen Ortsnamen und gibt uns ein `*time.Location` Objekt zurück. Dieses Objekt können wir dann beim Aufruf von `time.Date()` oder `time.Time()` nutzen, um das Datum und die Uhrzeit in der gewünschten Zone abzurufen.

## Siehe auch

Wir haben uns nun angesehen, wie man in Go das aktuelle Datum und die aktuelle Zeit abrufen kann. Für weitere Informationen über die `time` Paket in Go, schauen Sie sich bitte die offizielle Dokumentation an: 

- [Go Zeitformatierung](https://golang.org/pkg/time/#Time.Format)
- [Godoc Zeitpaket](https://godoc.org/time)

Happy coding!