---
date: 2024-01-26 04:14:06.185524-07:00
description: "Hur man g\xF6r: I Fish \xE4r den interaktiva skalet standardl\xE4get\
  \ n\xE4r du startar det. S\xE5 h\xE4r ser det ut i aktion."
lastmod: '2024-03-13T22:44:38.339680-06:00'
model: gpt-4-0125-preview
summary: "I Fish \xE4r den interaktiva skalet standardl\xE4get n\xE4r du startar det."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

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
