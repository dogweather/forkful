---
date: 2024-01-26 01:03:16.814951-07:00
description: "Logging ist im Grunde das Festhalten, was Ihre Anwendung gerade macht\
  \ \u2013 ein Tagebuch, wenn man so will, aber f\xFCr Code. Programmierer tun dies,\
  \ um den\u2026"
lastmod: '2024-02-25T18:49:51.366047-07:00'
model: gpt-4-1106-preview
summary: "Logging ist im Grunde das Festhalten, was Ihre Anwendung gerade macht \u2013\
  \ ein Tagebuch, wenn man so will, aber f\xFCr Code. Programmierer tun dies, um den\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?
Logging ist im Grunde das Festhalten, was Ihre Anwendung gerade macht – ein Tagebuch, wenn man so will, aber für Code. Programmierer tun dies, um den Überblick über die Details zu behalten, wie Zustandsänderungen, Systemereignisse und hartnäckige Fehler, und sicherzustellen, dass keine Störungen unbemerkt bleiben.

## Wie geht das:
In Fish kann das Protokollieren so einfach sein wie das Weiterleiten der Standardausgabe und Fehlerströme in eine Datei. Lassen Sie uns einen Log-Eintrag für die Start- und Endzeiten unseres Skripts erstellen.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skript gestartet" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skript beendet" >> my_app.log
end

log_start
# ... die Aufgaben Ihres Skripts ...
log_end

cat my_app.log
```

Hier ist, was Sie in `my_app.log` sehen würden:

```
2023-04-01 10:35:47  - Skript gestartet
2023-04-01 10:36:02  - Skript beendet
```

Für fortgeschrittenes Logging können Sie Funktionen mit Parametern für das Log-Level und Nachrichten nutzen:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Dies ist eine informative Nachricht."
log_message ERROR "Etwas ist schiefgelaufen!"
```

Beispielausgabe in `my_app.log` wäre:
```
2023-04-01 10:35:47 [INFO] Dies ist eine informative Nachricht.
2023-04-01 10:35:49 [ERROR] Etwas ist schiefgelaufen!
```

## Tiefer Eintauchen
Historisch gesehen wurde das Logging in Shell-Skripten mit einer Reihe von `echo`-Anweisungen durchgeführt, und obwohl dies sicherlich immer noch eine Option ist, kann die Implementierung komplexerer Systeme eine Herausforderung sein. Fish hat keinen eingebauten Logging-Mechanismus wie einige andere Shells oder Programmiersprachen, so dass Sie oft Ihr eigenes System entwickeln müssen.

Alternativen zum eingebauten `echo`-Befehl von Fish zum Protokollieren umfassen Unix-Tools wie `syslog` oder `logger`, die mit dem Systemprotokoll-Daemon interagieren und einen integrierteren Ansatz zum Protokollieren von systemweiten Ereignissen bieten.

Die Einfachheit von Fish ermöglicht es Ihnen, Funktionen zum Verwalten der Ausführlichkeit des Loggings zu erstellen und verschiedene Ebenen einzusetzen, die Sie ein- oder ausschalten können. Einige Implementierungen können sogar den Namen des Skripts, die Zeilennummer und den Zeitstempel enthalten, was das Zurückverfolgen der Schritte, die zu einem Ereignis geführt haben, erleichtert.

## Siehe auch
- Die Fish Shell Dokumentation zum Schreiben von Funktionen: https://fishshell.com/docs/current/#syntax-function
- Grundlagen der Shell-Scripting-Tipps: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Leitfaden zum Syslog-Protokoll: https://tools.ietf.org/html/rfc5424
