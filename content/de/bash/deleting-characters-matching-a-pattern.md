---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Bash: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Manchmal passiert es, dass man in einer Datei oder in einer Zeichenkette bestimmte Zeichen oder Muster löschen muss. Das kann zum Beispiel der Fall sein, wenn man Texte bereinigen oder Formatierungen entfernen möchte. In solchen Situationen ist es hilfreich zu wissen, wie man in Bash Zeichen und Muster löschen kann.

## Wie man Zeichen und Muster in Bash löschen kann

```Bash
# Löscht alle Leerzeichen aus einer Zeichenkette
echo "Hallo Welt!" | sed 's/ //g'
# Ausgabe: HalloWelt!

# Löscht alle Ziffern aus einer Datei
cat datei.txt | tr -d '[0-9]'
# Hinweis: Die Option -d gibt an, welche Zeichen gelöscht werden sollen. In diesem Fall sind es die Ziffern von 0 bis 9.
```

## Tiefer Einblick

Es gibt verschiedene Möglichkeiten, um in Bash Zeichen und Muster zu löschen. Eine davon ist die Verwendung der Befehle sed und tr. 

Der Befehl sed steht für "Stream Editor" und ermöglicht es, in Textdateien nach bestimmten Zeichen oder Mustern zu suchen und sie zu ersetzen oder zu löschen. Mit dem Befehl tr, der für "Translate" steht, können bestimmte Zeichen aus einer Datei in andere Zeichen umgewandelt oder gelöscht werden.

Es ist auch möglich, in Bash mit regulären Ausdrücken zu arbeiten, um Zeichen und Muster zu löschen. Ein regulärer Ausdruck ist eine Folge von Zeichen, die ein bestimmtes Muster beschreiben und zum Beispiel verwendet werden kann, um nach bestimmten Wörtern oder Zeichenketten zu suchen. In Kombination mit den Befehlen sed und tr können so gezielt Zeichen und Muster gelöscht werden.

## Siehe auch

- [Bash Grundlagen Tutorial (auf Deutsch)](https://wiki.ubuntuusers.de/Bash/): Ein ausführlicher Artikel über die Grundlagen von Bash, inklusive Informationen über reguläre Ausdrücke.
- [Sed & Awk: Eine Einführung für die Praxis (auf Deutsch)](https://www.linux-magazin.de/ausgaben/2010/05/sed-awk/): Ein Tutorial über die Verwendung von sed und awk in der Praxis.
- [Git Bash: Einführung und Tipps für die Praxis (auf Deutsch)](https://www.digitalocean.com/community/tutorials/einführung-in-die-verwendung-der-git-bash-zur-ausführung-von-shell-befehlen): Ein Artikel, der erklärt, wie man mit dem Befehlszeilentool Git Bash arbeitet und gleichzeitig Shell Befehle ausführt.