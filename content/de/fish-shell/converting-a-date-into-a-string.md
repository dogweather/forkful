---
title:                "Eine Datum in einen String umwandeln"
html_title:           "Fish Shell: Eine Datum in einen String umwandeln"
simple_title:         "Eine Datum in einen String umwandeln"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn du regelmäßig mit Daten arbeitest, wirst du irgendwann die Notwendigkeit haben, ein Datum in eine Zeichenfolge (String) umzuwandeln. Sei es für die Benennung von Dateien oder für die Ausgabe in einem Bericht, die Umwandlung eines Datums in einen String kann sehr hilfreich sein.

## Wie geht das?

Das Konvertieren eines Datums in einen String ist mit Fish Shell sehr einfach. Dafür gibt es die Funktion `string`, die das entsprechende Datum in einer benutzerdefinierten Zeichenfolgeformatierung ausgibt.

```Fish Shell
set date (date +%Y-%m-%d)
string $date +"Der heutige Tag ist %Y-%m-%d"
```

Die obige Funktion speichert das aktuelle Datum in der Variablen `date` und konvertiert es dann in einen String mit dem gewünschten Format. Die Ausgabe würde in diesem Fall wie folgt aussehen: "Der heutige Tag ist 2021-06-03".

Natürlich kannst du auch andere Formate verwenden, abhängig von deinen spezifischen Anforderungen. Hier sind einige Beispiele, die du ausprobieren kannst:

- `string $date +"%d.%m.%Y"` (Ausgabe: "03.06.2021")
- `string $date +"%b, %Y"` (Ausgabe: "Jun, 2021")
- `string $date +"Heute ist %A"` (Ausgabe: "Heute ist Donnerstag")

## Tiefergehende Informationen

Um das gewünschte Ergebnis zu erzielen, ist es wichtig, die richtige Formatierung zu verwenden. Im Grunde genommen steht jedem einzelnen Zeichen in deiner Formatierung eine bestimmte Information über das Datum zur Verfügung. Hier sind einige Beispiele für häufig verwendete Zeichen:

- `%Y` - Jahr mit vier Ziffern (z.B. 2021)
- `%m` - Monat mit führender Null (z.B. 06 für Juni)
- `%d` - Tag mit führender Null (z.B. 03)
- `%b` - Monatsname abgekürzt (z.B. Jun)
- `%B` - Monatsname ausgeschrieben (z.B. Juni)
- `%a` - Wochentag abgekürzt (z.B. Do)
- `%A` - Wochentag ausgeschrieben (z.B. Donnerstag)

Eine vollständige Liste aller verfügbaren Zeichen und ihrer Bedeutung findest du in der [Dokumentation von Fish Shell](https://fishshell.com/docs/current/).

## Siehe auch

- [Konvertieren von Zeichenfolgen in Daten in Fish Shell](https://fishshell.com/docs/current/commands.html#string-to-date-conversions)
- [Dokumentation zu Datum und Uhrzeit in Fish Shell](https://fishshell.com/docs/current/commands.html#date-and-time)