---
title:                "Fish Shell: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum es sinnvoll ist, sich mit dem Lesen von Befehlszeilenargumenten in der Fish Shell zu beschäftigen. Zum einen ermöglicht es eine bessere Kontrolle über die Ausführung von Skripten und zum anderen spart es Zeit, da das manuelle Eingeben von Parametern entfällt. Außerdem bietet die Fish Shell einige praktische Funktionen, die das Lesen von Befehlszeilenargumenten noch effizienter machen.

## Wie

Um Befehlszeilenargumente in der Fish Shell zu lesen, müssen Sie zunächst das `$argv`-Array verwenden. Dieses Array enthält alle als Argumente übergebenen Werte. Um zum Beispiel das erste Argument auszugeben, können Sie folgenden Befehl verwenden:
```
Fish Shell $argv[1]
```
Die Ausgabe entspricht dann dem ersten übergebenen Argument. Wenn Sie alle Argumente ausgeben möchten, können Sie eine Schleife verwenden:
```
Fish Shell for arg in $argv
    echo $arg
end
```
Dies gibt jedes Argument in einer neuen Zeile aus.

## Deep Dive

Es gibt noch weitere Funktionen, die das Lesen von Befehlszeilenargumenten in der Fish Shell erleichtern. Eine nützliche ist `argparse`, mit der Sie Argumente in einem definierten Format auslesen können. Dazu müssen Sie zuerst eine Beschreibung erstellen, die angibt, welche Argumente erwartet werden und wie sie ausgelesen werden sollen. Anschließend können Sie `argparse` verwenden, um die Argumente auszulesen. Ein Beispiel sieht folgendermaßen aus:
```
Fish Shell argparse -d "Dies ist eine Beschreibung" \
    i:int l:location o:output \
    or set -- $argv
```
In diesem Fall wird ein Integer-Argument (`i`), ein String-Argument (`l`) und ein weiteres String-Argument (`o`) erwartet. Sie können optional auch default-Werte für die Argumente angeben.

## Siehe auch

- Offizielle Dokumentation zur Fish Shell: https://fishshell.com/docs/current/
- "The Fish Shell Cookbook" mit nützlichen Beispielen und Tipps: https://fishshell.com/docs/current/index.html
- GitHub-Repository mit Beispielen und Erklärungen zur Verwendung von Befehlszeilenargumenten in der Fish Shell: https://github.com/jorgebarrero/fish-argparse-tutorial