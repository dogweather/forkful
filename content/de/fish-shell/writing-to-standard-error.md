---
title:                "Schreiben auf den Standardfehler"
html_title:           "Fish Shell: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Was & Warum?
Schreiben auf den Standardfehler oder "Standard-Error" ist ein Weg für Programmierer, um Fehlermeldungen und wichtige Informationen während der Laufzeit ihres Codes anzuzeigen. Dies ist besonders nützlich, wenn die Ausgabe auf dem Standardausgang oder "Standard-Output" für normale Ergebnisse verwendet wird.

# Wie geht's?
Die Fish Shell verfügt über eine integrierte Funktion namens "echoerr", mit der Sie auf den Standardfehler schreiben können. Verwenden Sie einfach den Befehl "echoerr" gefolgt von dem gewünschten Text, und er wird auf dem Standardfehler ausgegeben. Zum Beispiel:

```Fish Shell
echoerr "Oops, something went wrong."
```

Ausgabe:
```
Oops, something went wrong.
```

Sie können auch vorhandene Fehlermeldungen mit Ihren eigenen Texten kombinieren, indem Sie einfach den Befehl "echoerr" innerhalb eines "if" -Statements verwenden. Zum Beispiel:

```Fish Shell
if [ $num -lt 10 ]
  echoerr "Die Nummer ist zu klein."
end
```

Ausgabe:
```
Die Nummer ist zu klein.
```

# Tiefer tauchen
Das Schreiben auf den Standardfehler ist eine gängige Praxis in der Programmierung und hat seine Wurzeln in den Unix-Systemen. Es ist eine effektive Methode, um wichtige Informationen anzuzeigen und gleichzeitig die Ausgabe auf dem Standardausgang frei zu halten.

Wenn Sie Fish Shell nicht verwenden, können Sie stattdessen den Befehl "stderr" verwenden, um auf den Standardfehler zu schreiben. Einige andere Shells erfordern möglicherweise die Verwendung von ">" und "2>" , um auf den Standardfehler zu schreiben.

Die Fish Shell implementiert das Schreiben auf den Standardfehler unter Verwendung des Befehls "2>&1" , der den Standardfehler auf den Standardausgang umleitet. Dadurch können sowohl Fehlermeldungen als auch normale Ausgaben im selben Terminalfenster angezeigt werden.

# Siehe auch
- [Fish Shell Dokumentation] (https://fishshell.com/docs/current/index.html)
- [Einführung in die Befehlszeilenschnittstelle] (https://wiki.ubuntuusers.de/Einf%C3%BChrung_in_die_Befehlszeile/)
- [Kurze Geschichte von Unix] (https://www.computerhope.com/unix.htm)