---
title:    "PHP: Umwandlung eines Datums in einen String"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum
Das Konvertieren von einem Datum in einen String ist eine häufige Aufgabe in der Webentwicklung, insbesondere wenn es um die Anzeige von datumsbasierten Inhalten geht. Indem du ein Datum in einen String umwandelst, kannst du es leichter formatieren und in deinem Code verwenden.

## Wie
Um ein Datum in einen String umzuwandeln, gibt es in PHP die Funktion `date()`, die es ermöglicht, ein Datum in verschiedenen Formaten darzustellen. Hier ist ein Beispiel, wie man das aktuelle Datum in einem einfachen String ausgeben kann:

```PHP
echo date("d/m/Y");
```
Dies würde ein Datum im Format Tag/Monat/Jahr ausgeben, z.B. `24/08/2021`.

Wenn du das Datum in einem anderen Format ausgeben möchtest, kannst du die Formatierungszeichen in der `date()` Funktion anpassen. Hier sind einige Beispiele:

- `d` - Tag des Monats (zweistellig)
- `m` - Monat (zweistellig)
- `Y` - Jahr (vierstellig)
- `j` - Tag des Monats (einstellig)
- `n` - Monat (einstellig)
- `y` - Jahr (zweistellig)
- `F` - Monatsname (z.B. "August")
- `M` - Monatsname (z.B. "Aug")
- `l` - Wochentag (z.B. "Montag")
- `D` - Wochentag (z.B. "Mon")
- `h` - Stunden im 12-Stunden-Format (zweistellig)
- `H` - Stunden im 24-Stunden-Format (zweistellig)
- `i` - Minuten (zweistellig)
- `s` - Sekunden (zweistellig)
- `a` - AM / PM (z.B. "am" oder "pm")

Du kannst diese Formatierungszeichen beliebig kombinieren, um das Datum in dem von dir gewünschten Format darzustellen. Hier sind einige Beispiele:

```PHP
echo date("F j, Y"); // Ausgabe: August 24, 2021
echo date("l, F jS, Y"); // Ausgabe: Dienstag, August 24th, 2021
echo date("h:i a"); // Ausgabe: 09:30 am (für 9:30 Uhr)
```

## Deep Dive
Wenn du mehr über die `date()` Funktion erfahren möchtest, kannst du die [offizielle PHP-Dokumentation](https://www.php.net/manual/en/function.date.php) besuchen. Dort findest du eine ausführliche Liste aller verfügbaren Formatierungszeichen sowie Beispiele und Erklärungen.

Eine weitere nützliche Funktion ist `strtotime()`, mit der du einen String in ein Datum umwandeln kannst. Hier ist ein Beispiel:

```PHP
echo date("d/m/Y", strtotime("next Friday")); // Ausgabe: 27/08/2021 (für den nächsten Freitag)
```

Du kannst auch Kombinationen aus `date()` und `strtotime()` verwenden, um komplexere Datumsberechnungen durchzuführen.

## Siehe auch
- [Offizielle PHP-Dokumentation zu Datei- und Zeitzonenformate](https://www.php.net/manual/en/datetime.format.php)
- [Tutorial zu Datum und Uhrzeit in PHP von SitePoint (auf Englisch)](https://www.sitepoint.com/quick-guide-to-dates-in-php/)
- [Online-Datumsformatierer-Tool von FreeFormatter](https://www.freeformatter.com/php-date-time-format.html)