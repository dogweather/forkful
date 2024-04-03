---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:51.388379-07:00
description: "Wie geht das: In Elixir k\xF6nnen Sie Funktionen des `IO`-Moduls wie\
  \ `IO.puts/2` und `IO.warn/2` verwenden, um Nachrichten an den Standardfehler zu\
  \ schreiben."
lastmod: '2024-03-13T22:44:53.551988-06:00'
model: gpt-4-0125-preview
summary: "In Elixir k\xF6nnen Sie Funktionen des `IO`-Moduls wie `IO.puts/2` und `IO.warn/2`\
  \ verwenden, um Nachrichten an den Standardfehler zu schreiben."
title: Schreiben auf Standardfehler
weight: 25
---

## Wie geht das:
In Elixir können Sie Funktionen des `IO`-Moduls wie `IO.puts/2` und `IO.warn/2` verwenden, um Nachrichten an den Standardfehler zu schreiben:

```elixir
# Eine einfache Nachricht an stderr schreiben
IO.puts(:stderr, "Fehler: Etwas ist schiefgelaufen!")

# Verwendung von IO.warn, was semantisch mehr für Warnungen/Fehler steht
IO.warn("Warnung: Sie sind dabei, das Limit zu überschreiten!")
```

Beispielausgabe im Terminal für `IO.puts/2`:
```
Fehler: Etwas ist schiefgelaufen!
```

Für `IO.warn/2` wäre die Ausgabe ähnlich, aber `IO.warn/2` ist speziell für Warnungen konzipiert und könnte in zukünftigen Elixir-Versionen zusätzliche Formatierungen oder Verhaltensweisen beinhalten.

**Verwendung von Drittanbieter-Bibliotheken**

Obwohl die Standardbibliothek von Elixir normalerweise ausreicht, um die Ausgabe von Standardfehlern zu behandeln, könnten Sie Bibliotheken wie `Logger` für komplexere Anwendungen oder zur Konfiguration verschiedener Protokollebenen und Ausgaben nützlich finden.

Beispiel für die Verwendung von `Logger` zur Ausgabe einer Fehlermeldung:

```elixir
require Logger

# Logger so konfigurieren, dass die Ausgabe an stderr erfolgt
Logger.configure_backend(:console, device: :stderr)

# Schreiben einer Fehlermeldung
Logger.error("Fehler: Verbindung zur Datenbank konnte nicht hergestellt werden.")
```

Diese Einrichtung leitet die Ausgabe von `Logger` speziell an stderr, was nützlich ist, um die Fehlerprotokollierung von Standardprotokollnachrichten zu trennen.
