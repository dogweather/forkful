---
title:    "Bash: Lesen von Befehlszeilen-Argumenten"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Befehlszeilenargumenten ist eine wichtige Fähigkeit in der Bash-Programmierung, die es ermöglicht, spezifische Informationen an ein Skript zu übergeben. Dadurch können Skripte flexibler gestaltet und auf verschiedene Situationen angepasst werden.

## Wie geht man vor

Um Befehlszeilenargumente in einem Bash-Skript zu lesen, gibt es verschiedene Schritte, die man befolgen muss. Zunächst müssen die Argumente mit dem Befehl `getopts` eingelesen werden. Anschließend kann man sie in Variablen speichern und zur weiteren Verwendung nutzen.

Ein Beispiel für die Verwendung von Befehlszeilenargumenten in einem Skript könnte wie folgt aussehen:

```Bash
#!/bin/bash

# Einlesen der Befehlszeilenargumente
while getopts ":u:p:" option; do
    case $option in
        u) # Falls das Argument -u übergeben wurde, wird es in die Variable USERNAME gespeichert
            USERNAME=$OPTARG
            ;;
        p) # Falls das Argument -p übergeben wurde, wird es in die Variable PASSWORD gespeichert
            PASSWORD=$OPTARG
            ;;
    esac
done

# Ausgabe der eingelesenen Argumente
echo "Benutzername: $USERNAME"
echo "Passwort: $PASSWORD"
```

Wenn man dieses Skript mit den Befehlszeilenargumenten `-u Max -p abc123` ausführt, erscheint folgende Ausgabe:

```
Benutzername: Max
Passwort: abc123
```

## Tiefere Einblicke

Die `getopts`-Funktion gibt es schon seit langer Zeit in der Bash, jedoch hat sie einige Einschränkungen. Zum Beispiel können keine mehrstelligen Argumente eingelesen werden. Aus diesem Grund gibt es mittlerweile auch das Paket `getopt`, welches noch weitere Optionen bietet und die Handhabung von Befehlszeilenargumenten erleichtert.

Außerdem ist es wichtig zu beachten, dass Befehlszeilenargumente in Bash-Skripten auch als Umgebungsvariablen verfügbar sind. Diese können genutzt werden, wenn man sie in der Bash-Datei als solche deklariert, zum Beispiel mit `export ARGUMENT`.

## Siehe auch

- [Offizielle Dokumentation zu Befehlszeilenargumenten](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Inoffizielle Einführung in die Bash-Programmierung auf Deutsch](http://openbook.rheinwerk-verlag.de/shell_programmierung/002_001.html#s002001)
- [Paket getopt auf GNU.org](https://www.gnu.org/software/gettext/manual/html_node/getopt.html)