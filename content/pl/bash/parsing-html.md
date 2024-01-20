---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza HTML (parsing HTML) to proces wyodrębniania danych z kodu HTML. Robimy to, aby skutecznie manipulować, analizować i używać struktur danych zawartych w kodzie internetowego dokumentu.

## Jak to zrobić:

Bash nie jest najbardziej odpowiedni do parsowania HTML, ale możemy uzyskać pewne wyniki z użyciem narzędzi jak `grep`, `sed` lub `awk`.

```Bash
$ echo '<div class="klasa">Cześć, Świecie!</div>' | grep -oP '(?<=class="klasa">).*(?=</div>)'
Cześć, Świecie!
```

Lepiej uzyć specjalistycznych narzędzi jak `pup` lub `hxselect` z `HTML-XML-utils`.

```Bash
$ echo '<div class="klasa">Cześć, Świecie!</div>' | pup 'div.klasa text{}'
Cześć, Świecie!
```

## Dogłębna analiza

Parsowanie HTML w Bash jest nieortodoksyjne i trudne. Bash jest powłością (shellem) systemu Unix zaprojektowaną do uruchamiania poleceń, a nie do analizy dokumentów. 

Histerycznie wiele narzędzi Bash-a, takich jak `grep`, `sed`, i `awk` były używane do analizy danych, ale nie są idealne do analizy HTML ze względu na skomplikowaną naturę języka HTML. 

Alternatywą mogą być dedykowane narzędzia do parsowania HTML jak `pup` czy `hxselect` z `HTML-XML-utils`, które są dedykowane do analizy składniowej języka HTML. Utworzone specjalnie z myślą o HTML, od początku są projektowane, aby właściwie interpretować i manipulować tą skomplikowaną strukturą danych.

## Zobacz też

- Dokumentacja Bash: https://www.gnu.org/software/bash/manual/bash.html
- Man page dla `grep`: https://man7.org/linux/man-pages/man1/grep.1.html
- Dokumentacja `pup`: https://github.com/ericchiang/pup
- Dokumentacja `HTML-XML-utils`: https://www.w3.org/Tools/HTML-XML-utils/README