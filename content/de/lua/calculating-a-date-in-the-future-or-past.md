---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Lua: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Berechnen von Datumswerten in der Zukunft oder Vergangenheit ist eine nützliche Funktion in der Programmierung. Es ermöglicht uns, effektiv mit Terminen und Zeiträumen umzugehen und somit zeitbasierte Aktionen in unseren Programmen zu planen und auszuführen.

# Wie Geht's?
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, benötigen wir das aktuelle Datum, einen Zeitraum und eine Richtung (zukünftig oder vergangen). Wir können dies in Lua einfach mit Hilfe von mathematischen Operationen und eingebauten Funktionen wie `os.date()` und `os.time()` erreichen.

Beispiel für das Berechnen eines Datums in der Zukunft:
```Lua
-- Aktuelles Datum
local now = os.time()

-- Zeitraum von 1 Tag
local timeDiff = 24 * 60 * 60

-- Berechnung des Datums in der Zukunft (in Sekunden)
local futureDate = now + timeDiff

-- Konvertierung in ein lesbares Datum
print(os.date("%d.%m.%Y", futureDate)) -- Output: 17.05.2021
```

Beispiel für das Berechnen eines Datums in der Vergangenheit:
```Lua
-- Aktuelles Datum
local now = os.time()

-- Zeitraum von 1 Woche
local timeDiff = 7 * 24 * 60 * 60

-- Berechnung des Datums in der Vergangenheit (in Sekunden)
local pastDate = now - timeDiff

-- Konvertierung in ein lesbares Datum
print(os.date("%d.%m.%Y", pastDate)) -- Output: 10.05.2021
```

# Tieferes Eintauchen
Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit ist eine weit verbreitete Funktion in der Programmierung und ist besonders nützlich bei der Planung von wiederkehrenden Aufgaben oder beim Einhalten von Fristen.

Alternativ können wir auch Lua-Bibliotheken wie [Lua-date](https://github.com/Tieske/date) verwenden, die erweiterte Funktionen für die Arbeit mit Datumswerten anbieten.

Im Hintergrund verwendet Lua die [Epoche](https://de.wikipedia.org/wiki/Epoche_(Informatik)) als Referenzpunkt für Zeitangaben, basierend auf dem [UNIX-Zeitstempel](https://de.wikipedia.org/wiki/Unix-Zeit). Die Funktionen `os.date()` und `os.time()` arbeiten damit, indem sie die Anzahl der vergangenen bzw. vergangenen Sekunden seit der Epoche zurückgeben. 

# Sieh Dir Auch An
- [Tutorialspoint - Lua Datum und Zeit](https://www.tutorialspoint.com/lua/lua_date_time.htm)
- [Lua-Dokumentation - os.date() Funktion](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [Deine Anwendung - Einfaches Beispiel für das Berechnen von Datumsangaben](https://deineanwendung.de/blog/berechnung-von-datumswerten-in-lua)