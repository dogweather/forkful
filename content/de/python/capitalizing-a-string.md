---
title:    "Python: Einen String großschreiben"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Warum

Das Kapitalisieren von Zeichenketten ist eine häufige Aufgabe beim Programmieren mit Python. Es kann verwendet werden, um Texte für eine bessere Lesbarkeit oder zur Datenverarbeitung zu formatieren. In dieser Blog-Post werden wir uns genauer ansehen, wie man eine Zeichenkette in Python kapitalisieren kann und warum dies nützlich sein kann.

# Wie

Um eine Zeichenkette in Python zu kapitalisieren, gibt es zwei einfache Methoden: die eingebaute Funktion `upper()` und die `capitalize()`-Methode. Lassen Sie uns beide Methoden anhand von Beispielen betrachten:

```Python
text = "hallo welt"
print(text.upper())
# Output: HALLO WELT

print(text.capitalize())
# Output: Hallo welt
```

Wie Sie sehen können, wird durch die Verwendung der `upper()`-Funktion die gesamte Zeichenkette in Großbuchstaben umgewandelt, während in der `capitalize()`-Methode nur der erste Buchstabe der Zeichenkette in einen Großbuchstaben umgewandelt wird.

Wenn Sie jedoch sicherstellen möchten, dass die restlichen Buchstaben in Kleinbuchstaben bleiben, können Sie die `title()`-Methode verwenden, die den ersten Buchstaben jedes Worts in der Zeichenkette in einen Großbuchstaben umwandelt:

```Python
text = "hallo welt"
print(text.title())
# Output: Hallo Welt
```

# Tiefergehende Analyse

Bei der Verwendung der `upper()`-Funktion müssen Sie berücksichtigen, dass sie auch Buchstaben mit diakritischen Zeichen wie ä, ö oder ü in Großbuchstaben umwandelt. Dies kann in bestimmten Fällen zu unerwarteten Ergebnissen führen. Wenn Sie sichergehen möchten, dass nur die Buchstaben A-Z in Großbuchstaben umgewandelt werden, können Sie die `casefold()`-Methode verwenden, die auch alle diakritischen Zeichen entfernt:

```Python
text = "müller"
print(text.upper()) # unerwarteter Output: MüLLER
print(text.casefold().upper()) # korrekter Output: MULLER
```

Eine weitere wichtige Sache zu beachten ist, dass diese Methoden nur für englische Buchstaben funktionieren. Wenn Sie mit Buchstaben aus anderen Sprachen arbeiten, sollten Sie die `upper()`-Funktion durch die `upper()`-Methode ersetzen, und die `title()`- und `capitalize()`-Methoden können unerwartete Ergebnisse liefern.

# Siehe auch

- [Python-Dokumentation über die `str.upper()`-Funktion](https://docs.python.org/3/library/stdtypes.html#str.upper)
- [Python-Dokumentation über die `str.capitalize()`-Methode](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Python-Dokumentation über die `str.title()`-Methode](https://docs.python.org/3/library/stdtypes.html#str.title)