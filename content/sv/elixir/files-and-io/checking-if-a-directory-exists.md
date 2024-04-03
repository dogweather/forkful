---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:05.406878-07:00
description: "Hur man g\xF6r: Elixirs standardbibliotek erbjuder ett enkelt s\xE4\
  tt att kontrollera existensen av en katalog genom `File`-modulen. S\xE5 h\xE4r kan\
  \ du anv\xE4nda den."
lastmod: '2024-03-13T22:44:37.581957-06:00'
model: gpt-4-0125-preview
summary: "Elixirs standardbibliotek erbjuder ett enkelt s\xE4tt att kontrollera existensen\
  \ av en katalog genom `File`-modulen."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur man gör:
Elixirs standardbibliotek erbjuder ett enkelt sätt att kontrollera existensen av en katalog genom `File`-modulen. Så här kan du använda den:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Katalogen finns!"
else
  IO.puts "Katalogen finns inte."
end
```

Exempelutdata, med antagandet att katalogen inte finns:
```
Katalogen finns inte.
```

För mer avancerade filsysteminteraktioner, inklusive att kontrollera katalogens existens, kan du överväga att använda tredjepartsbibliotek som `FileSystem`. Även om Elixirs standardfunktioner är tillräckliga för många fall, kan `FileSystem` erbjuda mer nyanserad kontroll och feedback för komplexa applikationer. Dock, för det grundläggande behovet av att kontrollera om en katalog finns, rekommenderas det vanligtvis att hålla sig till den inbyggda `File`-modulen eftersom den är direkt tillgänglig och inte kräver några externa beroenden.
