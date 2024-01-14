---
title:                "Bash: Drucken von Fehlerausgaben"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Debugging ist eine unverzichtbare Fähigkeit für jeden Programmierer. Durch das Drucken von Debug-Ausgaben können Sie Schritt für Schritt den Ablauf Ihres Codes verfolgen und potenzielle Fehlerquellen identifizieren. Es ist ein nützliches Werkzeug, um komplexe Programmierprobleme zu lösen und die Effizienz Ihrer Arbeit zu verbessern.

# Wie man Debug-Ausgaben in Bash schreibt

Um Debug-Ausgaben in Bash zu schreiben, verwenden Sie einfach den Befehl `echo`, gefolgt von der Information, die Sie ausgeben möchten. Zum Beispiel:

```
echo "Dies ist eine Debug-Ausgabe"
```

Sie können auch Variablen in Ihren Debug-Ausgaben verwenden, um genauere Informationen über den Zustand Ihres Codes zu erhalten. Verwenden Sie in diesem Fall den Befehl `printf` und die Platzhalter `%s` oder `%d` für Zeichenketten bzw. Zahlen. Zum Beispiel:

```
name="Max Mustermann"
age=25
printf "Benutzer: %s | Alter: %d" $name $age
```

Dies würde folgende Ausgabe generieren:

```
Benutzer: Max Mustermann | Alter: 25
```

Debug-Ausgaben können auch in Schleifen verwendet werden, um den Wert von Variablen bei jedem Schleifendurchlauf anzuzeigen. Auf diese Weise können Sie Fehler schneller erkennen und beheben. Hier ist ein Beispiel:

```
for i in {1..10}
do
  echo "Aktueller Wert von i: $i"
done
```

Dies würde 10 Debug-Ausgaben generieren, die den aktuellen Wert von `i` bei jedem Schleifendurchlauf anzeigen.

# Tiefgehende Analyse

Es ist wichtig zu beachten, dass Debug-Ausgaben für temporäre Zwecke verwendet werden sollten und nicht Teil des endgültigen Codes sein sollten. Daher ist es ratsam, die Befehle `echo` oder `printf` auszukommentieren oder zu entfernen, sobald der Fehler behoben ist.

Eine andere Möglichkeit, Debug-Ausgaben zu erfassen, ist die Verwendung von `>>` und `2>&1` in Kombination mit dem Befehl `tee`. Dies ermöglicht es Ihnen, die Ausgabe Ihrer Debug-Ausgaben in eine separate Datei zu speichern, anstatt sie auf dem Bildschirm anzuzeigen. Zum Beispiel:

```
echo "Fehlermeldung" 2>&1 | tee debug.log
```

Dieser Befehl speichert die Debug-Ausgabe in der Datei `debug.log`, während sie gleichzeitig auf dem Bildschirm angezeigt wird.

# Siehe auch

- [Bash Debugging Techniken](https://www.linuxjournal.com/content/bash-debugging-techniques)
- [Debugging in Bash: Tipps und Tricks](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html)
- [Debugging Bash Skripte mit set -x](https://www.shell-tips.com/bash/debugging-bash-scripts/)