---
title:                "Textsuche und -austausch"
html_title:           "Fish Shell: Textsuche und -austausch"
simple_title:         "Textsuche und -austausch"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Was & Warum?

Suchen und Ersetzen von Text ist eine häufige Aufgabe, die Programmierer ausführen müssen. Es beinhaltet das Finden eines bestimmten Textabschnitts innerhalb eines größeren Textes und das Ersetzen durch einen anderen Text. Programmierer verwenden dies, um schnell Änderungen in ihrem Code vorzunehmen oder um Fehler zu beheben.

# Wie geht's:

Um Text in der Fish Shell zu suchen und zu ersetzen, können Sie das vi und sed Kommando verwenden. Im Folgenden finden Sie eine Beispielverwendung und die entsprechende Ausgabe:

```Fish Shell
# Öffne eine Datei mit vi
vi datei.txt

# Suche und ersetze "Hallo" durch "Hi"
:%s/Hallo/Hi/g

# Speichere die Änderungen und beende vi
:wq

# Zeige den Inhalt der Datei an
cat datei.txt

# Output:
Hi Welt
```

# Tiefergehende Details:
Die Suche und Ersetzung von Text hat eine lange Geschichte in der Programmierung und war schon in den frühen Tagen der Textverarbeitung von Bedeutung. Alternativen zu vi und sed sind z.B. das beliebte Tool awk, welches sich durch eine leistungsstarke Syntax auszeichnet. Die Implementierung von Suchen und Ersetzen in der Fish Shell basiert auf regulären Ausdrücken, die ein mächtiges Werkzeug sind, um Textmustern zu ermitteln und zu manipulieren.

# Weitere Informationen:
Weitere Informationen und Tutorials zur Verwendung von vi, sed und awk finden Sie auf den offiziellen Websites dieser Tools. Außerdem können Sie in der Fish Shell-Dokumentation mehr über die Verwendung von regulären Ausdrücken erfahren.