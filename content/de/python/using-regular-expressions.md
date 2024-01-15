---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Python: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein unverzichtbares Werkzeug für jeden Python-Entwickler. Sie ermöglichen es, komplexe Muster in Texten zu suchen und zu manipulieren, was die Programmierung von Textverarbeitungsaufgaben erheblich erleichtert. Durch die Verwendung regulärer Ausdrücke können Sie auch schnell und effizient Daten validieren und filtern.

## Wie geht das?

Reguläre Ausdrücke werden in Python mit dem Modul `re` verwendet. Um loszulegen, importieren Sie dieses Modul in Ihrem Code:

```Python
import re
```

Um einen regulären Ausdruck auf einen String anzuwenden, verwenden Sie die `re.search()` Funktion. Sie nimmt zwei Argumente an: den regulären Ausdruck als String und den Text, auf den er angewendet werden soll.

```Python
result = re.search(r'(\d+)-(\d+)-(\d+)', 'Heute ist der 25-06-2020')

# result entspricht dem regulären Ausdruck, wenn er im Text gefunden wird, sonst None
```

Um die gefundenen Übereinstimmungen zu extrahieren, verwenden Sie die `group()` Methode auf dem Ergebnisobjekt:

```Python
# Die Gruppe(1) entspricht dem Tag, Gruppe(2) dem Monat und Gruppe(3) dem Jahr
result.group(1) # 25
result.group(2) # 06
result.group(3) # 2020
```

Sie können auch reguläre Ausdrücke verwenden, um Texte zu ersetzen, indem Sie die `re.sub()` Funktion benutzen:

```Python
new_text = re.sub(r'Python', 'Schlange', 'Ich liebe Python!')

# new_text entspricht dem String "Ich liebe Schlange!"
```

## Tiefere Einblicke

Reguläre Ausdrücke unterstützen auch verschiedene Flags, die beim Erstellen des regulären Ausdrucks verwendet werden können, um das Verhalten zu ändern. Zum Beispiel können Sie die `IGNORECASE` Flag setzen, um die Groß- und Kleinschreibung zu ignorieren.

```Python
result = re.search('PYTHON', 'Ich lerne Python', flags = re.IGNORECASE)

# result entspricht dem regulären Ausdruck, unabhängig von der Groß- und Kleinschreibung
```

Es gibt auch spezielle Zeichenklassen, die verwendet werden können, um spezifische Arten von Zeichen in einem Text zu finden, wie z.B. Zahlen, Buchstaben oder Leerzeichen. Reguläre Ausdrücke ermöglichen auch das Definieren von Wiederholungen, z.B. das Suchen von einem oder mehreren Vorkommen eines bestimmten Musters.

## Siehe auch

- Dokumentation für `re` Modul in der offiziellen Python-Dokumentation: https://docs.python.org/3/library/re.html
- Reguläre Ausdrücke Tutorial von Real Python: https://realpython.com/regex-python/