---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:22.454498-07:00
description: "Wie geht das: In Fish Shell k\xF6nnen Strings direkt mit eingebauten\
  \ Funktionen manipuliert werden, ohne dass externe Tools oder Bibliotheken erforderlich\u2026"
lastmod: '2024-03-13T22:44:54.291576-06:00'
model: gpt-4-0125-preview
summary: "In Fish Shell k\xF6nnen Strings direkt mit eingebauten Funktionen manipuliert\
  \ werden, ohne dass externe Tools oder Bibliotheken erforderlich sind."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie geht das:
In Fish Shell können Strings direkt mit eingebauten Funktionen manipuliert werden, ohne dass externe Tools oder Bibliotheken erforderlich sind. Um einen String zu kapitalisieren, können Sie den `string`-Befehl mit Unterbefehlen kombinieren.

```fish
# Beispiel-String
set sample_string "hello world"

# Ersten Buchstaben großschreiben
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Ausgabe:
```
Hello world
```

Für Szenarien, die die Kapitalisierung mehrerer Wörter in einem String erfordern (z. B. die Umwandlung von "hello world" in "Hello World"), würden Sie über jedes Wort iterieren und die Kapitalisierungslogik auf jedes anwenden:

```fish
# Beispiel-Satz
set sentence "hello fish shell programming"

# Jedes Wort kapitalisieren
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Die kapitalisierten Wörter verbinden
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Ausgabe:
```
Hello Fish Shell Programming
```

Beachten Sie, dass Fish Shell keinen direkten Ein-Befehl-Ansatz für die vollständige Satzkapitalisierung bietet, wie es einige Programmiersprachen mit ihren String-Methoden tun. Daher stellt die Kombination von `string split`, `string sub`, `string upper` und anschließendem Wiederzusammenfügen einen idiomatischen Ansatz in Fish Shell dar, um dies zu erreichen.
