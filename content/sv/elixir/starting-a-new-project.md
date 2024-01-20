---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att starta ett nytt projekt innebär att skapa ett nytt program eller system från grunden. Programmerare gör detta för att lösa specifika problem eller för att bygga nya verktyg och egenskaper.

## Så här gör du:
Elixir erbjuder en mix-exekverbar för att generera projektets skel. För att starta ett nytt projekt, kör följande i din terminal:
```Elixir
mix new my_project
cd my_project
```
Detta skapar ett nytt Elixir-projekt med namnet "my_project". Kör projektet genom att använda `mix`:
```Elixir
mix run -e MyProject.start
```
## Djupdykning
Elixir är ett funktionellt, samtidigt och generellt programmeringsspråk som körs på Erlang virtuella maskin (BEAM). Det är utformat för att bygga skalbara och underhållbara applikationer. 

Alternativen till att starta ett nytt projekt i Elixir kan inkludera andra språk som Erlang, Rust eller Go. Valet beror dock på projektets specifika krav och det önskade arbetsflödet. 

När det gäller implementationsdetaljer kompilerar `mix new` alla källkoder i `lib` och inkluderar dem i BEAM-filerna. Det automatiserar många processer inklusive testning, taskhantering och projektgenerering, vilket bidrar till effektiv kodning.

## Se också
- Elixir officiella dokumentation: https://elixir-lang.org/docs.html
- Elixir Forum: https://elixirforum.com/
- Erlang Virtuell Maskin (BEAM) dokumentation: http://www.erlang.org/doc/