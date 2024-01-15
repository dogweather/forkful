---
title:                "Umwandeln eines Datums in einen String"
html_title:           "PHP: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wer mit PHP programmiert, kommt nicht darum herum, mit Datumsangaben zu arbeiten. Oft ist es nötig, diese Datumsangaben in Strings umzuwandeln, zum Beispiel um sie in einer bestimmten Formatierung auf der Webseite auszugeben. In diesem Artikel erfährst du, wie du das in PHP ganz einfach umsetzen kannst.

## Wie Geht's

Um ein Datum in einen String umzuwandeln, gibt es in PHP die Funktion `date()`, die folgendermaßen aufgebaut ist:

```PHP
$string = date ( string $format [, int $timestamp = time() ] );
```

Das bedeutet, dass die Funktion zwei Parameter erwartet: Das Format des Strings, in den das Datum umgewandelt werden soll, und optional ein Timestamp, der das Datum angibt, das umgewandelt werden soll. Wenn kein Timestamp angegeben wird, verwendet die Funktion automatisch das aktuelle Datum.

Als Beispiel wollen wir das aktuelle Datum in der Formatierung "Tag.Monat.Jahr" ausgeben. Dafür sieht der Code folgendermaßen aus:

```PHP
$string = date("d.m.Y");
echo $string;
```

Das Ergebnis wäre in diesem Fall zum Beispiel `02.03.2020`.

## Deep Dive

Das Format, in dem das Datum ausgegeben werden soll, wird mithilfe von Platzhaltern definiert. Zum Beispiel steht `d` für den Tag im Monat mit zwei Ziffern, `m` für den Monat mit zwei Ziffern und `Y` für das Jahr mit vier Ziffern. Eine vollständige Liste der verfügbaren Platzhalter findest du in der offiziellen PHP-Dokumentation.

Wenn du das aktuelle Datum nicht ausgeben möchtest, sondern ein festgelegtes Datum umwandeln möchtest, kannst du den zweiten Parameter der `date()`-Funktion verwenden. Dafür muss der Timestamp des gewünschten Datums angegeben werden. Dieser kann entweder als Integer-Wert oder mithilfe der Funktion `strtotime()` definiert werden.

## Siehe Auch

- [Offizielle PHP-Dokumentation zur date() Funktion](https://www.php.net/manual/de/function.date.php)
- [Übersicht über alle verfügbaren Platzhalter](https://www.php.net/manual/de/function.date.php#refsect1-function.date-parameters)