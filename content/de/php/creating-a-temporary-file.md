---
title:    "PHP: Erstellen einer temporären Datei"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum
Das Erstellen temporärer Dateien ist ein nützliches Konzept in der PHP-Programmierung, da es ermöglicht, Daten temporär zu speichern, ohne permanente Dateien erstellen zu müssen.

## Wie geht das?
Um eine temporäre Datei in PHP zu erstellen, können Sie die `tempnam()` Funktion verwenden. Diese Funktion benötigt einen Parameter, der den Pfad zum Ordner angibt, in dem die temporäre Datei erstellt werden soll. In den meisten Fällen werden Sie wahrscheinlich den systemweiten temporären Ordner verwenden wollen, der mit der `sys_get_temp_dir()` Funktion abgerufen werden kann.

Ein Beispielcode könnte wie folgt aussehen:

```PHP
$tempFileName = tempnam(sys_get_temp_dir(), "prefix_");
echo $tempFileName;
```

Dieser Code erstellt eine temporäre Datei mit dem Präfix "prefix_" und gibt den Dateinamen aus.

Die Ausgabe könnte beispielsweise lauten: "/tmp/prefix_zn23g92w1".

## Tiefere Einblicke
Wenn Sie sich genauer mit dem Erstellen temporärer Dateien befassen möchten, sollten Sie die `tempnam()` Funktion genauer untersuchen. Es gibt verschiedene Optionen, die Sie übergeben können, einschließlich der Möglichkeit, den Präfix und den Suffix der Datei anzugeben.

Darüber hinaus können Sie auch die `tmpfile()` Funktion verwenden, um eine temporäre Datei zu erstellen, die automatisch gelöscht wird, sobald das Skript beendet ist. Diese Funktion erfordert keine Parameter und gibt direkt einen geöffneten Dateizeiger zurück, mit dem Sie arbeiten können.

In jedem Fall ist es wichtig, dass Sie die temporären Dateien am Ende Ihres Skripts löschen, um sicherzustellen, dass Ihr System nicht unnötig mit temporären Dateien belastet wird.

## Siehe Auch
1. `tempnam()` Funktionsreferenz: https://www.php.net/manual/de/function.tempnam.php
2. `tmpfile()` Funktionsreferenz: https://www.php.net/manual/de/function.tmpfile.php