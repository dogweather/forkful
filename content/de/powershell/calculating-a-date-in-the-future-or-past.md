---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "PowerShell: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist das Erstellen eines Datums abhängig von der aktuellen Zeit plus oder minus eine bestimmte Zeitspanne. Entwickler tun dies oft, um Zeitintervalle zu handhaben, Erinnerungen und Benachrichtigungen zu programmieren oder Terminplanungs-Tools zu erstellen.

## Wie geht das:

In PowerShell ist dieses Thema dank zweier Hauptbefehle recht einfach: `Get-Date` (erhalten Sie das aktuelle Datum) und `AddDays` (fügen Sie eine bestimmte Anzahl von Tagen hinzu). Hier ist ein einfaches Beispiel:

```PowerShell
# Aktuelles Datum bekommen
$myDate = Get-date

# 10 Tage in die Zukunft gehen
$myFutureDate = $myDate.AddDays(10)

# Ausgabe
$myFutureDate
```

Nachdem Sie dieses Skript ausgeführt haben, würden Sie eine Ausgabe ähnlich dieser erwarten: 

```PowerShell
Donnerstag, 25. November 2021 10:00:00
```

Für die Vergangenheit verwenden Sie einfach eine negative Zahl:

```PowerShell
# 10 Tage in die Vergangenheit gehen
$myPastDate = $myDate.AddDays(-10)

# Ausgabe
$myPastDate
```

Das würde ähnlich aussehen:

```PowerShell
Sonntag, 5. November 2021 10:00:00
```

## Tiefer eintauchen:

Diese Methode zum Berechnen von Daten ist tief in den `DateTime`-Typen von PowerShell verankert, die selbst auf den .NET Framework `DateTime`-Typen basieren. Alternativen zu `AddDays` könnten `AddHours`, `AddMinutes`, `AddMonths` etc. sein, je nach Bedarf. 

Es ist wichtig zu beachten, dass diese Methoden immer eine neue `DateTime`-Instanz zurückgeben und das ursprüngliche `DateTime`-Objekt unverändert bleibt. Dies ist auf die Unveränderlichkeit der `DateTime`-Struktur im .NET Framework zurückzuführen.

## Mehr dazu:

Für zusätzliche Ressourcen und vertiefte Informationen könnten Sie folgende Links nützlich finden:

1. Offizielle Dokumentation für `Get-Date`: https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/get-date
2. Offizielle Dokumentation für `DateTime`: https://docs.microsoft.com/de-de/dotnet/api/system.datetime
3. Weitere Informationen zu `DateTime`-Metoden: https://docs.microsoft.com/de-de/dotnet/api/system.datetime.adddays