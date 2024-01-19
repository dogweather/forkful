---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Verkettung von Zeichenketten ist das Aneinanderfügen von zwei oder mehr Strings. Programmierer tun dies um komplexe Nachrichten zu erstellen, Dateinamen zu erstellen oder um Daten zu formatieren.

## Wie geht's:

Im Fish Shell verwenden wir das "echo" Kommando um Zeichenketten zu verketten. Hier ist ein einfacher Code:

```Fish Shell
set string1 "Hallo"
set string2 ", Welt"
echo $string1$string2
```
Das Ergebnis wäre:

```Fish Shell
Hallo, Welt
```

## Tiefere Einblicke

Die Verkettung von Strings hat einen historischen Kontext, der bis in die Anfänge der Programmierung zurückreicht. In Fish Shell, im Gegensatz zu anderen Shells wie BASH, gibt es keine spezielle Syntax für eine Zeichenkettenverkettung. Sie wird einfach durch das direkte Schreiben von Variablen nebenläufig erreicht.

Alternativ können Sie auch den "string join" Befehl verwenden, aber das ist in der Praxis weniger üblich. Es sieht so aus:

```Fish Shell
string join "" $string1 $string2
```

In Bezug auf die Implementierungsdetails, Fish Shell interpretiert einfach die direkte Nebeneinanderstellung von Zeichenketten als Anforderung zur Verkettung.

## Siehe Auch

Um mehr über Verkettung von Zeichenketten zu lernen, schauen Sie sich diese Ressourcen an:

1. [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
2. [String Join Doku](https://fishshell.com/docs/current/cmds/string-join.html)
3. [Tutorial zur Fish Shell Programmierung](https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell)