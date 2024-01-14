---
title:    "Fish Shell: Suchen und Ersetzen von Text"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie häufig mit der Bearbeitung von Texten oder Code arbeiten, wissen Sie wahrscheinlich, wie nützlich die Funktion zum Suchen und Ersetzen ist. Mit der Fish Shell können Sie diese Aufgabe schnell und einfach erledigen. Lesen Sie weiter, um zu erfahren, wie Sie Text in der Fish Shell suchen und ersetzen können.

## Wie geteilt

### Eine einfache Suche und Ersetzung durchführen
Die Fish Shell bietet die Befehle `grep` und `sed` zur Textsuche und -ersetzung. Um nach einem bestimmten Begriff in einem Text zu suchen und ihn zu ersetzen, können Sie den folgenden Befehl verwenden:

```
Fish Shell Code:

grep -l "zu-ersetzen-wort" dateiname | xargs sed -i "s/zu-ersetzen-wort/ersatz-text/"
```

In diesem Beispiel wird das Wort "zu-ersetzen-wort" im angegebenen Dateinamen gesucht und durch "ersatz-text" ersetzt. Beachten Sie, dass das `-i` Flag in `sed` verwendet wird, um die Datei direkt zu ändern. Wenn Sie die Änderungen in einer neuen Datei speichern möchten, können Sie stattdessen das Flag `-c` verwenden.

### Die Laufzeitumgebung von Fish in vim einbinden
Falls Sie den Texteditor vim verwenden, können Sie die Fish-Shell-Befehle auch direkt in vim verwenden, um Texte zu suchen und zu ersetzen. Fügen Sie einfach die folgenden Zeilen in Ihre vim-Konfigurationsdatei `~/.vimrc` ein:

```
Fish Shell Code:

" Die Laufzeitumgebung von Fish in vim einbinden
function! RunFish(cmd)
  let fish_output = system("fish -c '" . a:cmd . "'")
  return trim(fish_output)
endfunction
command! -nargs=1 F run RunFish('<args>')
```

Nachdem Sie die Datei gespeichert haben, können Sie die Befehle `:F grep` und `:F sed` verwenden, um eine Suche oder Ersetzung auszuführen.

## Tiefentauchgang
Die Fish Shell verwendet standardmäßig die `awk`-Sprache für Suche und Ersetzung, aber Sie können auch andere Sprachen wie `sed`, `perl` oder `grep` für komplexere Fälle verwenden. Sie können auch reguläre Ausdrücke verwenden, um die Suche und Ersetzung weiter zu verfeinern.

Eine weitere nützliche Funktion der Fish Shell ist das "Fishing" - ein interaktives Such- und Ersetzungstool. Um es zu verwenden, führen Sie einfach den Befehl `fish_vi_mode` aus und drücken Sie dann `Ctrl-R`. Geben Sie Ihren Suchbegriff ein und anschließend die Eingabetaste, um durch das Dokument zu navigieren und ihn zu ersetzen.

## Siehe auch
- Fish Shell Dokumentation über `grep` und `sed`: https://fishshell.com/docs/current/commands.html#grep
- vim Online-Hilfe zu Regulären Ausdrücken: https://vimhelp.org/pattern.txt.html