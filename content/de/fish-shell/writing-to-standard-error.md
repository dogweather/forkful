---
title:                "Fish Shell: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben in den Standardfehler oder "Standard Error" ist ein wichtiger Teil des Programmierens in der Fish Shell. Durch das Schreiben von Fehlern und Warnungen auf den Standardfehler können Sie Fehler in Ihrem Code identifizieren und beheben. Es ermöglicht Ihnen auch, Benutzerinnen und Benutzer über potenzielle Probleme oder unerwartete Ergebnisse zu informieren. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man in Fish Shell effektiv auf den Standardfehler schreibt.

## Wie geht man vor?

Um in Fish Shell auf den Standardfehler zu schreiben, können Sie die `echo`-Befehlsfunktion verwenden. Diese Funktion gibt den angegebenen Text auf den Standardausgang (Standard Output) oder den Standardfehler aus, je nach den angegebenen Parametern. Um auf den Standardfehler zu schreiben, müssen Sie das Flag `>&2` verwenden. Dies weist Fish Shell an, den Text auf den Standardfehler statt auf den Standardausgang zu schreiben.

```Fish Shell
echo "Dies ist eine Fehlermeldung" >&2
```

Die Verwendung von `>&2` nach dem Text ist wichtig, da Fish Shell es sonst wie einen Teil der Nachricht betrachtet und es auf den Standardausgang schreibt. Um sicherzustellen, dass Ihr Code richtig funktioniert, sollten Sie immer auf den Standardfehler schreiben, wenn Sie Fehler und Warnungen ausgeben.

Sie können auch den vorinstallierten Befehl `echo_err` verwenden, der speziell zum Schreiben auf den Standardfehler entwickelt wurde. Hier ist ein Beispiel:

```Fish Shell
echo_err "Dies ist eine weitere Fehlermeldung"
```

## Tiefergehende Informationen

Das Schreiben auf den Standardfehler wird in Fish Shell automatisch mit den Befehlen `status` und `echo_err` gemacht, wenn Sie Fehler oder Warnungen haben. Aber Sie können auch manuell auf den Standardfehler schreiben, indem Sie das `>&2` Flag verwenden, wie in diesem Artikel gezeigt wurde.

Es ist auch wichtig zu wissen, dass Fish Shell einen speziellen Statuscode zurückgibt, wenn auf den Standardfehler geschrieben wird. Dieser Code kann von anderen Programmen oder Skripten ausgelesen werden, um zu bestimmen, ob ein Fehler aufgetreten ist. Der Code wird `1` sein, wenn auf den Standardfehler geschrieben wurde, und `0`, wenn alles erfolgreich war.

## Siehe auch

Hier sind einige nützliche Links, die Ihnen helfen können, mehr über das Schreiben auf den Standardfehler in Fish Shell zu erfahren:

- Offizielle Fish Shell Dokumentation: https://fishshell.com/docs/current/cmds/echo_err.html
- Ein praktisches Tutorial zum Schreiben auf den Standardfehler: https://blog.typcodex.com/fishing-writing-to-standard-error-with-fish-shell/
- Die verschiedenen Möglichkeiten, Fehler in Fish Shell zu behandeln: https://www.2daygeek.com/redirect-linux-shell-script-errors-sterr-to-devnull-stdout/