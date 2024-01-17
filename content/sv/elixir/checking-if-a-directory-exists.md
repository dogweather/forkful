---
title:                "Kontrollera om en mapp finns"
html_title:           "Elixir: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns är en viktig del av programmering eftersom det hjälper till att säkerställa att koden kan hitta och använda nödvändiga filer och resurser på rätt plats.

## Hur man gör:
För att kontrollera om en mapp finns i Elixir kan du använda funktionen `File.exists?`. Detta returnerar antingen `true` eller `false` beroende på om mappen finns eller inte.

```Elixir
File.exists?("min_mapp") #=> true
File.exists?("ej_finns_mapp") #=> false
```

## Djupdykning:
Att kontrollera om en mapp finns är en viktig del av sättet Elixir hanterar filer. Tidigare språk som Erlang, vilket Elixir är baserat på, använde sig av en arvodelning av en enda filstruktur. Men i Elixir kan flera filstrukturen manipuleras och detta kan kräva kontroll på olika nivåer.

Om `File.exists?` inte fungerar för dina behov, finns alternativ som `File.real_dir?` vilket returnerar `true` eller `false` beroende på om den givna sökvägen är till en katalog eller inte.

För dem som är nyfikna här är några få implementationdetaljer kring hur `File.exists?`-funktionen fungerar. Den använder sig av primära operativsystemanrop som `stat` i Unix, vilket returnerar information om den givna filen. Detta används sedan för att bestämma om den är en katalog eller inte.

## Se även:
- Officiell Elixir dokumentation: https://hexdocs.pm/elixir/File.html#exists?
- Elixir Shell-escape modul (Erlangs 'erl_call'): https://erlang.org/doc/man/shell_escape.html