---
title:    "Java: Einen Termin in der Zukunft oder Vergangenheit berechnen"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Warum

Das Berechnen von Datum in der Zukunft oder Vergangenheit kann sehr nützlich sein, um Aufgaben und Termine zu planen oder um Vergleiche anzustellen. Mit Java können wir dies auf einfache Weise programmatisch umsetzen.

# Wie man das macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, benötigen wir die Klasse `Calendar` aus der Java Time Library. Diese Klasse ermöglicht es uns, mit Datum und Zeit zu arbeiten und verschiedene Operationen auszuführen.

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zunächst eine Instanz der `Calendar` Klasse erstellen, die das aktuelle Datum und die aktuelle Zeit enthält. Wir können dies mit Hilfe der statischen `getInstance()` Methode tun.

```Java
// Erstelle eine Instanz von Calendar mit dem aktuellen Datum und Uhrzeit
Calendar cal = Calendar.getInstance();
```

Als nächstes können wir mit der `add()` Methode ein bestimmtes Datum in der Zukunft oder Vergangenheit berechnen. Diese Methode nimmt zwei Parameter an - ein Feld, das angibt, welche Komponente des Datums wir ändern möchten (z.B. Jahr, Monat, Tag) und einen Wert, der die Anzahl der Einheiten angibt, um die das Datum verändert werden soll.

```Java
// Berechne ein Datum 5 Tage in der Zukunft
cal.add(Calendar.DAY_OF_MONTH, 5);
```

Um das berechnete Datum auszugeben, können wir die `getTime()` Methode verwenden, die ein `Date` Objekt zurückgibt. Wir können dann das `Date` Objekt zum Beispiel formatieren und ausgeben.

```Java
// Erstelle ein Date Objekt aus dem berechneten Datum
Date result = cal.getTime();

// Formatiere das Datum als String und gebe es aus
SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");
System.out.println("Das berechnete Datum ist: " + formatter.format(result));
```

Die Ausgabe würde wie folgt aussehen:

```
Das berechnete Datum ist: 22.07.2021
```

# Tiefergehende Informationen

Bei der `add()` Methode können wir verschiedene Felder angeben, um Datum und Zeit zu manipulieren. Eine vollständige Liste der verfügbaren Felder und deren entsprechenden Konstanten finden Sie in der Java-Dokumentation für die `Calendar` Klasse.

Außerdem müssen wir beachten, dass die `Calendar` Klasse ein veränderliches Objekt ist. Das bedeutet, dass wenn wir weitere Operationen auf dem berechneten Datum ausführen möchten (z.B. Subtrahieren von Tagen), wir die ursprüngliche `Calendar` Instanz verwenden müssen. Wenn wir allerdings das berechnete Datum als separate Variable speichern möchten, sollten wir das `Date` Objekt nutzen.

# Siehe auch

- Java-Dokumentation für die `Calendar` Klasse: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- Java-Dokumentation für die `Date` Klasse: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- Java Time Library Tutorial: https://www.baeldung.com/java-time-api