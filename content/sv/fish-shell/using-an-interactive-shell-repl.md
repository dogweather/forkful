---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:14:06.185524-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
REPL, eller Read-Eval-Print Loop, är en interaktiv programmeringsmiljö som tar enskilda användarinmatningar, exekverar dem och returnerar resultatet. Programmerare använder den för omedelbar återkoppling, felsökning och snabb experimentell utforskning av programmeringskoncept utan den overhead som kommer med att kompilera och köra ett fullständigt program.

## Hur man gör:
I Fish är den interaktiva skalet standardläget när du startar det. Så här ser det ut i aktion:

```Fish Shell
> set color blue
> echo "Himlen är $color"
Himlen är blå
```

Du kan också köra inbyggda funktioner och leka med kommandoersättningar:

```Fish Shell
> function cheer
      echo "Kör Hårt $argv!"
  end
> cheer Kodare
Kör Hårt Kodare!
```

Inte bara att definiera funktioner, du kan exekvera kodsnuttar direkt och se utmatningen omedelbart:

```Fish Shell
> math "40 / 2"
20
```

## Djupdykning
Konceptet med REPLs går långt tillbaka till Lisp-programmeringsspråket på 1960-talet. Denna form av interaktiv programmering satte riktmärket för miljöer som Pythons `ipython` och Rubys `irb`. Fish fortsätter trenden med fokus på användarvänlighet och interaktiv användning.

Fish skiljer sig från andra skal som Bash genom att det från början är designat med interaktivitet i åtanke. Det tillhandahåller syntaxmarkering, autosuggesteringar och tabbkompletteringar som gör det kraftfullt att använda i en REPL-stil arbetsflöde. Ännu bättre, dina kommandon kommer ihåg och är sökbara, vilket gör upprepad testning till en barnlek.

Alternativ till Fishs REPL kan vara `bash` eller `zsh` när de paras ihop med tillägg som `bash-completion` eller `oh-my-zsh`, men Fish tenderar att erbjuda en rikare upplevelse direkt ur lådan.

## Se även:
- Fish dokumentation: https://fishshell.com/docs/current/index.html
- En intressant jämförelse mellan Fish och andra skal: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- En djupare dykning i REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Interaktiv programmering i Lisp, en historisk titt: http://www.paulgraham.com/ilisp.html