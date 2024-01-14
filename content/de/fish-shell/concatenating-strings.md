---
title:    "Fish Shell: Verbinden von Zeichenketten"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Wenn Du dich mit dem Fish Shell beschäftigt hast, bist du vielleicht schon einmal auf das Konzept des "Kettenverknüpfens" oder "String Concatenation" gestoßen. Aber warum sollte man überhaupt Strings verknüpfen? Nun, es gibt verschiedene Gründe dafür. Zum einen kann es helfen, längere Ausdrücke oder Variablen in kürzere und übersichtlichere Formen zu bringen. Außerdem wird das Verknüpfen von Strings oft verwendet, um dynamische Inhalte zu erzeugen, wie z.B. beim Erstellen von Dateinamen oder beim Ausgeben von Benutzerantworten.

# Wie man es macht

Das Verknüpfen von Strings ist im Fish Shell relativ einfach. Man verwendet den Operator `+` und die Syntax ist folgende:

```Fish Shell
string1 + string2
```

Lasst uns das anhand eines einfachen Beispiels demonstrieren:

```Fish Shell
$ set a "Hallo"
$ set b "Welt"
$ echo $a
Hallo
$ echo $b
Welt
$ echo $a$b
HalloWelt
```

Wie du siehst, werden die beiden Strings einfach aneinandergehängt. Wenn du Leerzeichen zwischen den beiden Strings haben möchtest, füge sie einfach hinzu:

```Fish Shell
$ set a "Hallo"
$ set b "Welt"
$ echo $a" "$b
Hallo Welt
```

# Tieferer Einblick

Wenn du dich tiefer mit der String-Konkatenierung im Fish Shell beschäftigen möchtest, gibt es einige weitere Funktionen, die du nutzen kannst. Zum Beispiel gibt es die Befehle `string join` und `string split`, die es ermöglichen, mehrere Strings miteinander zu verknüpfen oder einen String in mehrere Teile zu zerlegen.

Außerdem kannst du mit dem Befehl `string replace` Teile eines Strings durch andere ersetzen. Ein Beispiel hierfür wäre das Ausgeben von Benutzerantworten, bei denen bestimmte Wörter durch Smileys ersetzt werden sollen.

# Siehe auch

Hier sind einige nützliche Links, die dir weitere Informationen über das Verknüpfen von Strings im Fish Shell liefern können:

- Offizielle Dokumentation des Fish Shell: https://fishshell.com/docs/current/
- Eine ausführliche Anleitung zur Verwendung von Strings im Fish Shell: https://www.informaticar.org/blog/concatenating-strings-in-fish-shell/
- Weitere nützliche Tipps und Tricks für das Arbeiten mit dem Fish Shell: https://www.twilio.com/blog/using-fish-shell-like-a-pro