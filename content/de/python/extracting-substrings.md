---
title:                "Python: Substrings extrahieren"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings, auch als Substrings bezeichnet, ist ein wichtiges Konzept in der Programmierung, das bei vielen Projekten nützlich sein kann. Mit dieser Technik können Sie bestimmte Teile eines Strings auswählen, die Ihren Bedürfnissen entsprechen, wie zum Beispiel bestimmte Wörter oder Zeichenfolgen.

## Wie geht man vor

Das Extrahieren von Substrings in Python ist relativ einfach mit der integrierten Funktion `substring()`. Um dies zu tun, geben Sie den ursprünglichen String und die gewünschten Indizes ein, die den Beginn und das Ende des Substrings angeben. Hier ist ein Beispiel:

```Python
string = "Hallo, mein Name ist Maria."
sub = string[13:18]

print(sub)
```

Dieser Code würde den Substring `Maria` aus dem ursprünglichen String extrahieren und ausgeben. Beachten Sie, dass die Indexierung in Python bei 0 beginnt, daher entspricht der Wert 13 dem ersten Zeichen des Substrings und 18 dem letzten Zeichen, das nicht mehr Teil des Substrings ist.

Das Extrahieren von Substrings kann auch nützlich sein, um einen Teil eines größeren Strings zu ersetzen. Dazu können Sie den Substring durch einen anderen String mit der Funktion `replace()` ersetzen.

## Tiefere Einblicke

Das Extrahieren von Substrings ist nicht nur in Python eine wichtige Fähigkeit, sondern wird auch in vielen anderen Programmiersprachen verwendet. Es ist auch möglich, mehrere Substrings aus einem String mit `split()` zu extrahieren, der den String in eine Liste von Teilstrings aufteilt. Darüber hinaus ist es auch möglich, eine bestimmte Anzahl von Zeichen aus einem String mit `ljust()` oder `rjust()` auszurichten.

Es ist wichtig zu beachten, dass Strings in Python unveränderlich (immutable) sind, was bedeutet, dass sie nach ihrer Erstellung nicht mehr geändert werden können. Bei der Extraktion eines Substrings aus einem String wird tatsächlich eine neue Kopie des Substrings erstellt.

## Siehe auch

- [Python Strings](https://www.python.org/dev/peps/pep-0249/#python-strings)
- [String Methods in Python](https://www.w3schools.com/python/python_ref_string.asp)
- [Geeignete Python Kurse](https://www.udemy.com/topic/python-programming-language/)