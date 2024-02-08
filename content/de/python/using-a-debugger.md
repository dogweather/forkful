---
title:                "Einsatz eines Debuggers"
aliases:
- de/python/using-a-debugger.md
date:                  2024-01-26T04:08:54.596903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
"Ein Debugger verwenden" bedeutet, Schritt für Schritt durch Ihren Python-Code zu gehen, um Bugs zu entdecken und das Verhalten zu verstehen. Wir tun das, weil es wesentlich einfacher ist, als einfach zu raten, wo die Dinge schiefgelaufen sind, und es erspart uns Stunden in der Hölle der Print-Statements.

## Wie geht das:
Lassen Sie uns die Verwendung von `pdb`, dem eingebauten Debugger von Python, durchgehen. Stellen Sie sich eine Datei, `buggy.py`, mit einem schwer zu findenden Fehler vor:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Wenn Sie dieses Skript ausführen, erwarten Sie `8`, aber es wirft nur einen Syntaxfehler. Es ist Zeit für den Debugger!

In Ihrem Terminal führen Sie aus:
```bash
python -m pdb buggy.py
```

Sie gelangen in den Debugger, und es sieht so aus:
```Python
> /pfad_zur_datei/buggy.py(1)<module>()
-> def add_one(number):
```

Benutzen Sie `l(ist)`, um mehr Code zu sehen, `n(ext)`, um zur nächsten Zeile zu gehen, oder `c(ontinue)`, um das Skript weiter auszuführen. Wenn Sie auf den Fehler stoßen, wird `pdb` stoppen und Sie untersuchen lassen.

Nachdem Sie `number ++ 1` zu `number + 1` korrigiert haben, starten Sie den Debugger neu, um die Korrektur zu testen.
Merken Sie sich, Freunde lassen Freunde nicht ohne Netz codieren. Genug gesagt.

## Tiefer Graben
Zurück in den dunklen Zeiten der Programmierung (auch bekannt als bevor integrierte Entwicklungsumgebungen, oder IDEs, überall waren), waren Debugger oft eigenständige Tools, die Sie außerhalb Ihres Texteditors verwenden würden. Sie kamen zur Rettung, indem sie Programmierern erlaubten, den Zustand ihrer Software an verschiedenen Ausführungspunkten zu inspizieren.

Stand 2023 ist Pythons `pdb` nicht das einzige Spiel in der Stadt. Leute könnten IDEs wie PyCharm oder Visual Studio Code verwenden, die ihre eigenen schicken Debugger integriert haben. Diese fügen praktische Funktionen hinzu, wie Breakpoints, die Sie mit einem Klick setzen können, anstatt kryptische Befehle zu tippen.

Dann gibt es noch `ipdb`, ein mit pip installierbares Paket, das die `IPython`-Güte zum Debuggen bringt. Es ist wie `pdb` auf Leistungsverstärkern, mit Tab-Vervollständigung und Syntax-Hervorhebung.

Debugger variieren auch in ihrer Implementierung. Einige nähern sich der Programmausführung auf Maschinen- oder Bytecode-Ebene. Andere, wie viele Debugger für Hochsprachen, führen den Code in einer speziellen Umgebung aus, die den Zustand von Variablen überwacht und den Ausführungsfluss steuert.

## Siehe auch
Für die vollständige Übersicht über Pythons eigenen Debugger, schauen Sie hier nach:
- Die `pdb`-Dokumentation: https://docs.python.org/3/library/pdb.html

Wenn Sie neugierig auf Alternativen sind, werden Ihnen diese Links weiterhelfen:
- `ipdb`-Repository und Benutzerhandbuch: https://github.com/gotcha/ipdb
- Debuggen mit Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- PyCharm-Debugging-Funktionen: https://www.jetbrains.com/help/pycharm/debugging-code.html

Frohe Fehlersuche!
