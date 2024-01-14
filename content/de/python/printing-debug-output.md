---
title:                "Python: Ausgabe von Debugging-Informationen"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Drucken von Debug-Ausgaben kann eine nützliche Methode sein, um Fehler in Ihrem Code zu finden. Durch das Hinzufügen von zusätzlichen Ausgaben können Sie den Verlauf der Ausführung Ihres Programms verfolgen und potenzielle Probleme identifizieren.

## Wie man es macht

Um Debug-Ausgaben in Python zu erstellen, verwenden Sie die `print()` Funktion. Hier ist ein Beispiel, wie Sie es in Ihrem Code verwenden können:

```Python
# Beispiel für Debug-Ausgabe
name = "Max Mustermann"
print("Der Name ist: " + name)
```

Dies würde die folgende Ausgabe erzeugen:

`Der Name ist: Max Mustermann`

Sie können auch Variablen oder Ausdrücke in der `print()` Funktion verwenden, um deren Werte auszugeben. Zum Beispiel:

```Python
# Beispiel für die Verwendung von Variablen in Debug-Ausgabe
jahr = 2021
print("Das aktuelle Jahr ist: ", jahr)
```

Dies würde die folgende Ausgabe erzeugen:

`Das aktuelle Jahr ist: 2021`

## Tiefere Einblicke

Wenn Sie weitere Informationen in Ihre Debug-Ausgaben einbinden möchten, können Sie formatierte Zeichenfolgen verwenden. Diese ermöglichen es Ihnen, Variablenwerte in einen String einzufügen. Zum Beispiel:

```Python
# Beispiel für die Verwendung von formatierten Zeichenfolgen
name = "Marie"
alter = 25

print("Mein Name ist {} und ich bin {} Jahre alt".format(name, alter))
```

Dies würde die folgende Ausgabe erzeugen:

`Mein Name ist Marie und ich bin 25 Jahre alt`

Sie können auch benannte Platzhalter verwenden, um den Code lesbarer zu machen. Zum Beispiel:

```Python
# Beispiel für die Verwendung von benannten Platzhaltern
name = "Müller"
alter = 35

print("Mein Name ist {name} und ich bin {age} Jahre alt".format(name=name, age=alter))
```

Dies würde die folgende Ausgabe erzeugen:

`Mein Name ist Müller und ich bin 35 Jahre alt`

## Siehe auch

Weitere Informationen zur `print()` Funktion und Debugging-Techniken finden Sie in den folgenden Ressourcen:

- [Die offizielle Python-Dokumentation zur print() Funktion](https://docs.python.org/de/3/library/functions.html#print)
- [Ein Tutorial zur Verwendung von Debug-Ausgaben in Python](https://realpython.com/python-debugging-pdb/)
- [Tipps zum Debuggen von Python-Code](https://www.instructure.com/canvas/resources/guides/how-advise-students-debugging-python)