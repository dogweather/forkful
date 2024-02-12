---
title:                "Zeichenketten verknüpfen"
aliases:
- /de/bash/concatenating-strings/
date:                  2024-01-20T17:33:58.012649-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Konkatenation ist das Aneinanderhängen von Zeichenketten. Programmierer nutzen das, um Inhalte dynamisch zu generieren, Daten zu formatieren oder einfach Infos zusammenzubringen.

## How to:

Hier siehst du, wie's gemacht wird – kurz und schmerzlos.

```Bash
# Variablenzuweisung
string1="Hallo"
string2="Welt"

# Direkte Konkatenation
greeting="${string1}, ${string2}!"
echo $greeting
```

Ausgabe:

```
Hallo, Welt!
```

Zwei Variablen kombinieren? Kein Problem:

```Bash
# Mit Variablen
kombiniert="${string1}${string2}"
echo $kombiniert
```

Ausgabe:

```
HalloWelt
```

## Deep Dive

Konkatenation ist alt wie die Programmierung selbst. Ursprünglich musste man oft langwierige Umwege nehmen. Heute? Ein Kinderspiel.

Alternativen? Klar, da wären zum Beispiel `printf` oder Heredocs, aber für simples Zusammensetzen von Strings braucht's meist keine Geschütze.

Was passiert unter der Haube? Nicht viel – der Interpreter legt die Strings einfach hintereinander im Speicher ab. Aber Achtung mit Sonderzeichen – ohne Anführungszeichen kann's unerwartete Ergebnisse geben.

## See Also

Mehr Infos? Hier entlang:

- Bash Reference Manual: https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
