---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-19
simple_title:         "Kontrollera om en katalog finns"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns handlar om att verifiera dess existens i filsystemet. Programmerare gör detta för att undvika fel vid filhantering, till exempel när de ska läsa från eller skriva till filer i mappen.

## Hur man gör:
Elixir använder `File` modulen för att interagera med filsystemet. Här är ett exempel:

```elixir
if File.dir?("min_mapp") do
  IO.puts("Mappen finns!")
else
  IO.puts("Mappen finns inte.")
end
```

Om "min_mapp" existerar, skriver konsolen ut "Mappen finns!", annars "Mappen finns inte."

## Djupdykning:
Funktionen `File.dir?/1` introducerades i Elixir för att ge enkel tillgång till vanliga filoperationer. Det finns alternativ som `:filelib.is_dir/1` från Erlangs standardbibliotek, men `File.dir?/1` är mer lättanvänt i Elixir-program.

Att kontrollera mappars existens är direkt relaterat till operativsystemets API och dess filsystem. Effektiviteten av sådana anrop kan variera beroende på underliggande system.

I historiskt sammanhang har denna funktionalitet alltid varit grundläggande för programmering eftersom den påverkar hur program läser in och sparar data. Det är också en säkerhetsfråga, då program som blindt litar på filresurser kan orsaka fel eller säkerhetsbrister.

## Se även:
- Elixir's `File` modul dokumentation: https://hexdocs.pm/elixir/File.html
- Erlang's `filelib` dokumentation: http://erlang.org/doc/man/filelib.html
- Förståelse kring filsystem och operativsystem: https://en.wikipedia.org/wiki/File_system
