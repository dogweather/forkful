---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, ist ein Vorgang, bei dem aus einer Zeichenkette alle Zeichen entfernt werden, die einem spezifischen Muster entsprechen. Programmierer machen das, um nicht benötigten Rauschen aus Daten zu beseitigen und ihre String-Verarbeitungsaufgaben effizienter zu gestalten.

## So geht's:
Sie können die `tr -d`-Befehlskombination verwenden, um ein Muster abzugleichen und zu löschen. Hier ist ein Beispiel:

```Bash
echo "Hallo, Welt!" | tr -d 'a'
# Ausgabe: "Hllo, Welt!"
```

Hier wird das 'a' aus dem String "Hallo, Welt!" entfernt.

```Bash
echo "123abc456" | tr -d '[:alpha:]'
# Ausgabe: "123456"
```

Hier werden alle alphabetischen Zeichen aus dem String "123abc456" entfernt, sodass nur noch die Zahlen übrig bleiben.

## Tiefer Tauchen
Dieser Ansatz zum Löschen von Zeichen, die einem Muster entsprechen, ist aus historischer Sicht ein Grundbestandteil der Unix-Philosophie der Textmanipulation und -verarbeitung. Es gibt Alternativen wie `sed` oder `awk`, die leistungsfähiger, aber auch komplexer sind. 

Unter der Haube ruft `tr -d` die Bibliotheksfunktion `strpbrk` auf, um die erste Übereinstimmung im String zu finden. Wenn eine Übereinstimmung gefunden wird, wird der Rest des Strings verschoben, um die Lücke zu füllen, und das Verfahren wird wiederholt.

## Siehe auch
- [GNU Bash Referenzhandbuch](https://www.gnu.org/software/bash/manual/bash.html)
- [GNU `tr` Befehlsdokumentation](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Sed - Ein Stream Editor](https://www.gnu.org/software/sed/manual/sed.html)
- [Awk - Musterabgleich und Verarbeitungssprache](https://www.gnu.org/software/gawk/manual/gawk.html)