---
title:    "Fish Shell: Schreiben auf Standardfehler"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben in die Standardfehlerausgabe ist ein wichtiges Konzept, das jeder, der mit der Fisch-Shell arbeitet, kennen sollte. Die Standardfehlerausgabe ist ein spezieller Ausgabestrom, der für Fehlermeldungen und Diagnoseinformationen verwendet wird. Durch das Schreiben in die Standardfehlerausgabe können Fehler effizienter erkannt und behoben werden.

## How To

Um in die Standardfehlerausgabe zu schreiben, können wir das `echo` Kommando verwenden, gefolgt von dem Zeichen `&2`, das angibt, dass die Ausgabe in die Standardfehlerausgabe gehen soll. Ein Beispiel dafür wäre:

```Fish Shell 
echo "Dies ist eine Fehlermeldung" >&2
```

Dies wird dazu führen, dass die Nachricht "Dies ist eine Fehlermeldung" in die Standardfehlerausgabe geschrieben wird.

Wir können auch Variablen oder Befehlsausgaben in die Standardfehlerausgabe schreiben. Hier ist ein Beispiel, bei dem wir die Standardfehlerausgabe verwenden, um den Inhalt einer Datei zu überprüfen:

```Fish Shell
if test -e "example.txt"
    echo "Datei gefunden" >&2
else
    echo "Datei nicht gefunden" >&2
end
```

Die Ausgabe wäre abhängig davon, ob die Datei "example.txt" vorhanden ist oder nicht.

## Deep Dive

Ein interessantes Merkmal der Fisch-Shell ist, dass sie standardmäßig die Inhalte der Standardfehlerausgabe und der Standardausgabe kombiniert. Dies kann jedoch zu Problemen führen, wenn wir nur die Inhalte der Standardausgabe sehen möchten. Um dies zu vermeiden, können wir den Modifizierer `2>&1` verwenden, um die Standardfehlerausgabe an die Standardausgabe anzuhängen. Dies führt dazu, dass die Standardfehlerausgabe mit der Standardausgabe kombiniert wird und wir nur die Inhalte der Standardausgabe sehen.

```Fish Shell
ls -l "example.txt" 2>&1 | more
```

Dies führt dazu, dass das Ergebnis des Befehls `ls -l "example.txt"` in die Standardfehlerausgabe geschrieben wird, wo es an die Standardausgabe angehängt wird und dann an das Piping-Programm "more" übergeben wird.

## Siehe auch

- Fish Shell Dokumentation: https://fishshell.com/docs/current/
- Vergleiche: Standardfehlerausgabe und Standardausgabe in Shell-Skripten: https://linuxize.com/post/redirect-stderr-stdout/
- Tutorials für die Verwendung der Fisch-Shell: https://www.digitalocean.com/community/tutorials/fish-shell-interactive-command-line-tutorial

  ___
  
  # Siehe auch

Verwenden von Umgebungsvariablen in der Fish-Shell: https://fishshell.com/docs/current/tutorial.html#tut_environment