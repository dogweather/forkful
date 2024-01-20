---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Nybörjarguide till att Starta ett Nytt Projekt i Gleam

## Vad & Varför?
Att starta ett nytt projekt innebär att strukturera en ny kodbas för att lösa specifika uppgifter. Programmerare gör detta för att förstå, experimentera och generera lösningar till nya problem.

## Så gör du:
Låt oss säga att du vill skapa ett nytt Gleam-projekt kallat "hej_verden".

```Gleam
$ rebar3 new gleam_lib hej_verden
===> Writing hej_verden/src/hej_verden.app.src
===> Writing hej_verden/src/hej_verden.gleam
```
Med dessa kommandon skapar du en ny Gleam-app i en katalog som heter "hej_verden".

Nu, testa din nya app.

```Gleam
$ cd hej_verden
$ rebar3 eunit
```
Om allt står rätt till borde du se ett meddelande som säger att allt gick bra.

## Djupdykning
Historiskt sett, kommer Gleam från landskapet av Erlang-beam-språk, inspirerade av den starka säkerheten hos statiskt typade språk. Det finns andra sätt att starta ett projekt, till exempel 'make' eller 'cmake', men 'rebar3' är det mest använda verktyget inom Erlang/Gleam gemenskapen. På implementationssidan genererar rebar3 en grundläggande katalogstruktur som paketerar all nödvändig kod och konfigurationer för din Gleam-app.

## Se även 
För mer information eller djupare studier om Gleam och rebar3, besök följande resurser:

- Gleam's officiella webbplats: [Gleam's officiella webbplats](https://gleam.run/)
- Rebar3's officiella dokumentation: [Rebar3's officiella dokumentation](https://www.rebar3.org/docs)
- Introduktion till Rebar3: [How to build with Rebar3](https://adoptingerlang.org/docs/development/setup/)
  
Gör dig redo att dyka in i världen av Gleam!