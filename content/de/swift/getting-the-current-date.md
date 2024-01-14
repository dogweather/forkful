---
title:                "Swift: Das aktuelle Datum erhalten"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist ein wichtiges Element unserer täglichen Programmierarbeit. Es kann verwendet werden, um Zeitstempel zu erstellen, Ereignisse in unserer App zu verfolgen, oder um einfach nur die aktuelle Uhrzeit anzuzeigen. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man in Swift das aktuelle Datum bekommt und welche Optionen es dafür gibt.

## Wie man das aktuelle Datum in Swift bekommt

```Swift
let date = Date()
print(date)
```

Der obige Code erstellt eine neue Instanz der Klasse Date, die das aktuelle Datum und die Uhrzeit enthält. Dies kann beliebig oft wiederholt werden, um das Datum zu aktualisieren.

Wenn wir jedoch nur das aktuelle Datum ohne die Uhrzeit benötigen, gibt es eine sogenannte Calendar-Klasse, die uns dabei hilft.

```Swift
let calendar = Calendar.current
let date = Date()

let components = calendar.dateComponents([.year, .month, .day], from: date)
print(components)
```

Der obige Code gibt uns das aktuelle Datum im Format von Jahr, Monat und Tag aus.

Wir können auch die Zeitzone für das aktuelle Datum festlegen, indem wir die Eigenschaft timeZone der Date-Klasse verwenden.

```Swift
let date = Date()
print(date)
date.timeZone = TimeZone.current
print(date)
```

## Tiefgreifende Informationen über das aktuelle Datum

Der Date-Klasse hat viele nützliche Methoden, die uns dabei helfen, das aktuelle Datum zu bearbeiten und auszugeben.

Zum Beispiel können wir mit der Methode date(byAdding:to:) das aktuelle Datum um eine bestimmte Anzahl von Tagen, Monaten oder Jahren verändern.

```Swift
let date = Date()
let changedDate = Calendar.current.date(byAdding: .day, value: 7, to: date)
print(changedDate)
```

Es ist auch möglich, das aktuelle Datum in ein anderes Format umzuwandeln, indem wir die date(withFormattedString:) Methode verwenden und einen String als Argument übergeben.

```Swift
let date = Date()
let formattedString = date.date(withFormattedString: "dd.MM.yyyy")
print(formattedString)
```

Es lohnt sich, sich mit den verschiedenen Möglichkeiten der Date-Klasse auseinanderzusetzen, um das aktuelle Datum optimal zu nutzen.

## Siehe auch

* [Apple Dokumentation über die Date-Klasse](https://developer.apple.com/documentation/foundation/date)
* [Swift Standard Library Documentation - Date](https://developer.apple.com/documentation/swift/date)