---
title:                "Schreiben auf Standardfehler"
aliases:
- /de/fish-shell/writing-to-standard-error/
date:                  2024-02-03T19:33:13.424864-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf den Standardfehler (stderr) in der Fish Shell bedeutet, Fehlermeldungen oder Diagnosen getrennt von der Standardausgabe (stdout) zu leiten. Programmierer tun dies, um sicherzustellen, dass Fehlerinformationen leicht identifiziert, verwaltet oder umgeleitet werden können, was den Debugging- und Logging-Prozess erleichtert.

## Wie geht das:

In der Fish Shell können Sie auf stderr schreiben, indem Sie Ihre Ausgabe mit `>&2` umleiten. Hier ist ein einfaches Beispiel:

```fish
echo "Dies ist eine Fehlermeldung" >&2
```

Dieser Befehl gibt einfach eine Nachricht auf stderr statt auf stdout aus. Wenn Sie ein Skript schreiben würden, das sowohl reguläre als auch Fehlermeldungen ausgibt, könnten Sie so etwas tun:

```fish
echo "Der Prozess wird gestartet"
echo "Ein Fehler ist aufgetreten" >&2
echo "Prozess abgeschlossen"
```

Beispielhafte Ausgabe, wenn Sie das Skript ausführen und stderr in eine Datei umleiten:

```
Der Prozess wird gestartet
Prozess abgeschlossen
```

Die Fehlermeldung würde nicht in der Standardausgabe erscheinen, sondern könnte in der Datei gefunden werden, in die Sie stderr umgeleitet haben.

In Szenarien, die eine ausgefeiltere Fehlerbehandlung oder Protokollierung erfordern, verfügt Fish nicht über eingebaute Bibliotheken, die speziell dafür konzipiert sind. Sie können jedoch externe Tools nutzen oder Funktionen schreiben, um zu helfen. Zum Beispiel könnte das Erstellen einer einfachen Logging-Funktion so aussehen:

```fish
function log_error
    echo $argv >&2
end

log_error "Dies ist eine fortgeschrittene Fehlermeldung"
```

Diese Funktion `log_error` nimmt jeden String, den Sie ihr geben, und schreibt ihn auf stderr. Die Verwendung von Funktionen wie dieser kann dabei helfen, Ihre Fehlerbehandlung sauber und konsistent in Ihren Skripten zu halten.
