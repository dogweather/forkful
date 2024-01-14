---
title:    "Python: Lesen von Befehlszeilenargumenten"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein nützliches Werkzeug für Python-Programmierer, um Benutzereingaben direkt von der Kommandozeile zu erhalten. Dies kann besonders hilfreich sein, wenn Sie Skripte schreiben, die auf verschiedene Eingaben reagieren müssen, ohne dass jede Eingabe einzelnen definiert werden muss. Es spart Zeit und macht den Code flexibler.

## Wie man es macht

Um Befehlszeilenargumente in Python zu lesen, verwenden Sie das `sys` Modul. Zuerst muss dieses Modul importiert werden. Dann können Sie auf die Argumente über `sys.argv` zugreifen, wobei der Index 0 den Pfad zur aktuellen Datei und die folgenden Indexe die Argumente enthalten.

```Python
import sys

# Beispielprogramm
print("Dieses Programm akzeptiert zwei Befehlszeilenargumente.")
print("Das erste Argument ist:", sys.argv[1])
print("Das zweite Argument ist:", sys.argv[2])
```

Im obigen Beispiel können Sie sehen, wie einfach es ist, auf Befehlszeilenargumente zuzugreifen. Sie können auch überprüfen, ob die richtige Anzahl an Argumenten übergeben wurde, indem Sie `len(sys.argv)` verwenden.

## Tiefer Einblick

Es gibt auch andere Module in Python, die das Lesen von Befehlszeilenargumenten erleichtern, wie zum Beispiel `argparse` oder `click`. Diese bieten zusätzliche Funktionen wie das Angeben von Argumenttypen oder das Erstellen von Hilfe- und Dokumentationsanweisungen. Es lohnt sich, sich mit diesen Modulen weiter zu beschäftigen, um die Funktionalität zu erweitern und den Code noch benutzerfreundlicher zu gestalten.

## Siehe auch

- [Python Dokumentation zu sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Einführung in das argparse Modul](https://realpython.com/command-line-interfaces-python-argparse/)
- [Dokumentation zu click](https://click.palletsprojects.com/en/8.0.x/)