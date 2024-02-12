---
title:                "Fehlerbehandlung"
aliases:
- /de/bash/handling-errors/
date:                  2024-01-26T00:36:56.268023-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?

Fehlerbehandlung in Bash-Skripten bedeutet, vorherzusehen, wo Probleme auftreten könnten, und diese elegant zu behandeln. Warum? Nun, es hält Ihr Skript robust und bewahrt die Benutzer davor, sich den Kopf zu zerbrechen, wenn Dinge nicht wie erwartet funktionieren.

## Wie geht das:

```Bash
#!/bin/bash

# Umleitung von stderr in eine Datei
grep "etwas" datei.txt 2> fehler.log

# Fehlerbehandlung mit Exit-Status
if ! grep "etwas" datei.txt; then
    echo "Hoppla, beim Suchen nach 'etwas' ist ein Fehler aufgetreten."
    exit 1
fi

# Verwendung von 'trap' zum Aufräumen vor dem Beenden bei einem Fehler
cleanup() {
  echo "Bereinige temporäre Dateien..."
  rm temp_*
}

trap cleanup ERR

# beabsichtigter Fehler: Datei existiert nicht
cat temp_datei.txt
```

Beispielausgabe, wenn ein Fehler auftritt:

```
Bereinige temporäre Dateien...
cat: temp_datei.txt: Datei oder Verzeichnis nicht gefunden
```

## Vertiefung

Die Fehlerbehandlung in Bash-Skripten geht zurück auf die Ursprünge der Unix-Shell, wo robuste und zuverlässige Skripte für die Systemadministration und Automatisierung (und sind) unerlässlich waren.

Traditionell werden Fehler in Bash behandelt, indem der Exit-Status eines Befehls überprüft wird, der konventionsgemäß 0 für Erfolg und einen von Null verschiedenen Wert für ein Scheitern zurückgibt.

Bash führte den Befehl `trap` als eingebaute Funktion ein, die es Benutzern erlaubt, Befehle anzugeben, die bei verschiedenen Signalen oder beim Skriptende ausgeführt werden. Dies ist nützlich für Aufräumaufgaben oder als letztes Mittel für die Fehlerbehandlung.

Es gibt auch den Befehl `set`, der das Verhalten von Bash bei Fehlern ändert. Zum Beispiel bewirkt `set -e`, dass ein Skript sofort beendet wird, wenn ein beliebiger Befehl mit einem von Null verschiedenen Status beendet wird, eine Möglichkeit, schnell zu scheitern und sich verschlimmernde Fehler zu vermeiden.

Alternativen zur eingebauten Fehlerbehandlung von Bash umfassen das explizite Überprüfen auf die Existenz von Dateien, die Verwendung von Befehlssubstitution oder sogar das Schreiben eigener Funktionen, um Fehler detaillierter zu behandeln.

Obwohl sorgfältige Fehlerbehandlung manchmal für kleine Skripte übertrieben erscheint, ist es eine Praxis, die viel Zeit beim Debuggen sparen kann und unerwartetes Verhalten sowohl für Sie als auch für die Benutzer verhindert.

## Siehe auch

- Bash-Handbuch zu Shell-Parametern: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Abschnitt über Fehlerbehandlung im Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/exit-status.html
- Ein ausführlicher Leitfaden zu `trap`: https://mywiki.wooledge.org/SignalTrap

Denken Sie daran, Skripten ist eine Kunstform, und wie Sie mit den Fehlern und Stolpersteinen umgehen, kann Ihr Meisterwerk widerstandsfähiger machen. Frohes Skripten!
