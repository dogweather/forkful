---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

**## Vad och varför?**

Att starta ett nytt projekt handlar om att börja på en ny kodningsresans från början, med ett specifikt mål i åtanke. Programmers skapar nya projekt när de behöver konstruera en unik lösning från grunden, eller när de vill lära sig ny teknik på djupet.

**## Så här gör du:**

Att starta ett nytt Clojure-projekt är enkelt med "Leiningen" verktyget. 

```clojure
;; Installation av Leiningen
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod a+x lein
mv lein /usr/local/bin/lein
```

När du har installerat Leiningen kan du skapa ett nytt projekt:

```clojure
lein new app mitt-projekt
```
"mitt-projekt" är här namnet på ditt nya projekt.

**## Djupdykning**

Att starta ett nytt projekt har sina rötter i de dagar då programmering var en långsam process, med behov av stora planerings- och övervakningssystem. Nu, dock, med moderna verktyg som Clojure och Leiningen, kan programmerare logiskt portionera deras arbete i mindre, hanterbara projekt.

Ett alternativ till att starta ett nytt projekt från grunden är att bifoga till ett befintligt projekt. Detta är ofta snabbare men kan skapa komplexitet om den ursprungliga koden inte är väl skriven eller underhållen.

Implementation av ett nytt projekt i Clojure är kraftfullt, eftersom språket ger uttrycksfull syntax och datastrukturer, parallell programmering stöd och ett starkt ekosystem av bibliotek.

**## Se också:**

[Leiningen Guide](https://leiningen.org/#install)
[Clojure Documentation](https://clojure.org/guides/getting_started)