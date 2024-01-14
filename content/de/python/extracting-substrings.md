---
title:    "Python: Extrahieren von Teilstrings."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

In der Programmierung ist es oft erforderlich, Teile von Texten oder Zeichenfolgen zu extrahieren. Das kann aus verschiedenen Gründen nützlich sein, zum Beispiel bei der Datenanalyse oder beim Aufteilen von Benutzereingaben. In diesem Artikel werden wir uns genauer damit befassen, wie man mithilfe von Python-Code Substrings extrahieren kann.

## Wie Geht's

Um Substrings in Python zu extrahieren, verwenden wir die Methode `substring()` und geben als Parameter den Start- und Endindex an, zwischen denen der Substring extrahiert werden soll. Hier ist ein Beispielcode für eine E-Mail-Adresse:

```Python
email = "example@domain.com"
username = email.substring(0, 6)
domain = email.substring(7, 15)
print(username)
print(domain)
```

Die Ausgabe wäre:

```
example
domain
```

Wir können auch negative Indizes verwenden, um vom Ende des Strings aus zu zählen. Zum Beispiel würde `email.substring(-3)` die letzten drei Zeichen des Strings zurückgeben.

## Tief Tauchen

Es gibt viele weitere Möglichkeiten, Substrings in Python zu extrahieren. Zum Beispiel können wir die Methode `split()` verwenden, um einen String an einem bestimmten Trennzeichen aufzuteilen und dann auf bestimmte Teile der resultierenden Liste zuzugreifen. Wir können auch reguläre Ausdrücke verwenden, um nach bestimmten Mustern im String zu suchen und Substrings zu extrahieren.

## Siehe Auch

- [Python String Methods](https://www.w3schools.com/python/python_strings.asp)
- [Python Regular Expressions](https://www.w3schools.com/python/python_regex.asp)
- [Python String Splitting](https://www.programiz.com/python-programming/methods/string/split)