---
title:                "Teilstrings extrahieren"
html_title:           "Bash: Teilstrings extrahieren"
simple_title:         "Teilstrings extrahieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings ist eine Technik, um Teile eines Strings zu isolieren und zu verwenden. Diese Methode wird von Programmierern verwendet, um Daten aus bestimmten Bereichen oder Formaten zu extrahieren und zu verarbeiten.

## Wie geht's?
Extrahieren von Teilstrings kann in Bash mit dem Befehl `cut` durchgeführt werden, indem man den gewünschten Teil des Strings angeben. Zum Beispiel: `cut -c 1-5 input.txt` wird die ersten fünf Zeichen jeder Zeile in der Datei "input.txt" ausgeben. Dies kann auch mit regulären Ausdrücken mithilfe des Befehls `grep` durchgeführt werden. Zum Beispiel: `grep -o '[0-9]\{3\}' input.txt` wird alle dreistelligen Zahlen in der Datei "input.txt" ausgeben.

## Tief tauchen
Das Extrahieren von Teilstrings hat eine lange Geschichte und wird in vielen Programmiersprachen verwendet. Es gibt auch Alternativen zu `cut` und `grep`, wie zum Beispiel `sed` und `awk`, die noch mehr Funktionen bieten. Die Implementierung dieser Methoden kann je nach Programmiersprache variieren, aber das Konzept bleibt das gleiche: Teilstrings werden isoliert und verwendet.

## Siehe auch
Weitere Informationen und Beispiele zum Extrahieren von Teilstrings können in der offiziellen Dokumentation von Bash gefunden werden unter https://www.gnu.org/software/bash/manual/html_node/Bash-Examples.html#Bash-Examples. Weitere Ressourcen und Tutorials finden Sie auf Websites wie https://linuxize.com/post/bash-extract-substring/.