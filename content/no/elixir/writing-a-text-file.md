---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
simple_title:         "Skriving av en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil er å lagre data til en fil som leses som tekst. Programmerere gjør dette for å lagre konfigurasjoner, resultater eller for å dele data mellom programmer.

## Hvordan:
```Elixir
# Skrive til en fil
File.write!("hello.txt", "Hei, verden!")

# Append til en fil
File.write!("hello.txt", "\nLegg til denne linjen.", mode: :append)
```
Resultat:
```
Hei, verden!
Legg til denne linjen.
```

## Dybdesynk:
Historisk sett brukte programmerere lavnivåsystemoperasjoner for filskriving. Alternativer inkluderer databasebruk eller nettverksbaserte løsninger. Implementasjonsdetaljer for filskriving i Elixir håndterer binær og UTF-8 koding, og sikrer kompatibilitet med forskjellige operativsystemer.

## Se Også:
- [Elixir's File module documentation](https://hexdocs.pm/elixir/File.html)
- [Intro to Elixir for new programmers](https://elixir-lang.org/getting-started/introduction.html)
- [IO and the file system in Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
