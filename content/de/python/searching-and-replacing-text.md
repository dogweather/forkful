---
title:                "Suchen und Ersetzen von Text"
html_title:           "Python: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Was & Warum?
Bei der Suche und dem Ersatz von Text geht es darum, in einem Text nach bestimmten Begriffen zu suchen und diese dann durch andere Begriffe zu ersetzen. Programmierer nutzen dies häufig, um ihre Codebase zu aktualisieren oder um schnellere und effizientere Methoden zu implementieren.

# So geht's:
Um Text in Python zu suchen und zu ersetzen, gibt es verschiedene Methoden. Eine Möglichkeit ist die Verwendung der ```replace()``` -Funktion. Mit dieser Funktion können Sie einen String in einem anderen String suchen und durch einen neuen String ersetzen. Zum Beispiel:

```
initial_string = "Ich mag Python!"
new_string = initial_string.replace("mag", "liebe")
print(new_string)
```
Ausgabe:
```
Ich liebe Python!
```

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken mit der ```re``` -Bibliothek. Mit regulären Ausdrücken können Sie spezifische Muster in einem Text finden und ersetzen. Zum Beispiel:

```
import re
initial_string = "Hallo Welt!"
new_string = re.sub("H[a-z]+", "Guten", initial_string)
print(new_string)
```
Ausgabe:
```
Guten Welt!
```

# Tiefere Einblicke:
Die Verwendung von regulären Ausdrücken hat ihre Wurzeln in der Mathematik und wurde später von Ken Thompson für die Programmiersprache Unix entwickelt. Heutzutage gibt es auch verschiedene textsuchende und -ersetzende Programme wie Sed und AWK, die auf Unix basieren. Alternativ können auch externe Textbearbeitungsprogramme wie Notepad++ oder Sublime Text verwendet werden.

# Siehe auch:
- [Offizielle Dokumentation für die replace() -Funktion in Python](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Offizielle Dokumentation für die re -Bibliothek in Python](https://docs.python.org/3/library/re.html)
- [Sed Dokumentation](http://www.gnu.org/software/sed/manual/sed.html)
- [AWK Dokumentation](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Notepad++](https://notepad-plus-plus.org/)
- [Sublime Text](https://www.sublimetext.com/)