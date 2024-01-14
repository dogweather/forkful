---
title:                "Elixir: Kontrollera om en mapp finns"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Även om Elixir är ett robust och pålitligt programmeringsspråk, är det alltid viktigt att kontrollera om en mapp eller katalog finns innan du försöker skriva eller läsa filer i den. Att göra detta kan hjälpa till att undvika oväntade fel och felaktiga resultat i dina program. I denna blogginlägg kommer vi att titta på hur man kan kontrollera om en mapp finns i Elixir.

## Hur man gör det

Att kontrollera om en mapp finns i Elixir är enkelt med hjälp av den inbyggda funktionen `File.dir?/1`. Denna funktion tar en sökväg som argument och returnerar `true` om mappen finns och `false` om den inte gör det.

```Elixir
iex> File.dir?("./mapp")
true

iex> File.dir?("./okänd_mapp")
false
```

För att göra saker ännu enklare kan vi använda Operatören `&&` för att kombinera denna funktion med `File.exists?/1`, som kontrollerar om en fil eller mapp finns. På så sätt kan vi kontrollera både existensen av mappen och att det faktiskt är en mapp.

```Elixir
iex> File.exists?("./mapp") && File.dir?("./mapp")
true

iex> File.exists?("./fil") && File.dir?("./fil")
false
```

Nu kan vi enkelt använda denna metod för att kontrollera om en mapp finns innan vi försöker manipulera den i vårt program.

## Djupdykning

Det är värt att notera att `File.exists?/1` och `File.dir?/1` endast returnerar `true` om det objekt som kontrolleras exakt matchar sökvägen. Det innebär att om du till exempel använder en relativ sökväg kommer funktionerna att returnera `false` eftersom sökvägen inte matchar exakt.

Det är också möjligt att använda `File.cwd/0` för att hämta den aktuella arbetskatalogen och sedan använda `Path.join/2` för att bygga en giltig sökväg att kontrollera.

```Elixir
iex> File.cwd
"/hem/användarnamn/projekt"

iex> path = Path.join(File.cwd, "mapp")
"/hem/användarnamn/projekt/mapp"

iex> File.dir?(path)
true
```

Du kan också använda funktionen `Path.expand/2` för att översätta förkortningar som `~` till en fullständig sökväg.

```Elixir
iex> Path.expand("~/mapp")
"/hem/användarnamn/mapp"
```

Med hjälp av dessa funktioner och metoder kan du enkelt kontrollera om en mapp finns i Elixir och undvika potentiella fel i ditt program.

## Se även

- [Elixir Dokumentation: File.mod(metadata)] (https://hexdocs.pm/elixir/File.html#has_dir/1)
- [Elixir Dokumentation: Path.join(path1, path2)] (https://hexdocs.pm/elixir/Path.html#join/2)
- [Elixir Dokumentation: Path.expand(path, base)] (https://hexdocs.pm/elixir/Path.html#expand/2)