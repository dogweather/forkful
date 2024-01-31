---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, mit denen man Text suchen und bearbeiten kann. Programmiere nutzen sie, um effizient Daten zu filtern, zu ersetzen oder zu extrahieren.

## How to:
```Fish Shell
# Suche nach dem Wort 'Fisch' in 'Ich habe einen Fisch im Teich.'
echo "Ich habe einen Fisch im Teich." | string match -r 'Fisch'
# Ausgabe: Fisch

# Ersetze 'Fisch' durch 'Karpfen'
echo "Ich habe einen Fisch im Teich." | string replace 'Fisch' 'Karpfen'
# Ausgabe: Ich habe einen Karpfen im Teich.

# Extrahiere Zahlen aus einem String
echo "Im Jahr 2023 ist Fish die Shell der Wahl." | string match -r '[0-9]+'
# Ausgabe: 2023
```

## Deep Dive
Reguläre Ausdrücke (RegEx) wurden in den 1950ern von Stephen Cole Kleene entwickelt. Alternativen zu RegEx sind parsers oder spezifische Textverarbeitungstools, aber RegEx ist wegen seiner Flexibilität und Effizienz einzigartig. In Fish Shell verwendet man `string match` um mit RegEx zu arbeiten, was direkt in die Shell integriert ist und keine externen Programme wie `grep` benötigt.

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Reguläre Ausdrücke (RegEx) – Ein schneller Einstieg](https://www.regular-expressions.info/)
- [GNU Grep Documentation](https://www.gnu.org/software/grep/manual/grep.html)
