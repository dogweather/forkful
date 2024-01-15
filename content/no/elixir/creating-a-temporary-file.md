---
title:                "Opprette en midlertidig fil"
html_title:           "Elixir: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når man jobber med programmering, kan det være nyttig å opprette midlertidige filer. Dette kan være for å lagre midlertidige data eller behandle informasjon før den lagres permanent. I Elixir er det enkelt å opprette midlertidige filer, noe som gjør det til et nyttig verktøy for programmører.

## Slik gjør du det

For å opprette en midlertidig fil i Elixir, kan du bruke funksjonen `File.temp_file!/1`. Denne funksjonen tar imot en liste med valgfrie argumenter, inkludert mappen hvor den midlertidige filen skal opprettes og et prefiks for filnavnet.

La oss se på et eksempel:

```Elixir
{:ok, temp_file} = File.temp_file!([dir: "/midlertidig", prefix: "temp_"])
```

Dette vil opprette en midlertidig fil med prefikset "temp_" i mappen "/midlertidig". Vi kan deretter skrive data til denne filen ved hjelp av funksjonen `IO.write/2`:

```Elixir
IO.write(temp_file, "Dette er midlertidig data")
```

Vi kan også lese data fra den midlertidige filen ved hjelp av funksjonen `File.read/1`:

```Elixir
data = File.read(temp_file)
```

For å slette den midlertidige filen, kan vi bruke funksjonen `File.rm/1`:

```Elixir
File.rm(temp_file)
```

Dette vil slette filen fra systemet vårt.

## Utforske dypere

I tillegg til de grunnleggende funksjonene som nevnt ovenfor, kan du også bruke Elixir til å opprette midlertidige mapper ved hjelp av funksjonen `Dir.tmpdir/0`. Denne funksjonen vil returnere banen til en midlertidig mappe som du kan bruke til å lagre midlertidige filer og data.

En annen nyttig funksjon er `File.temp_dir/1` som oppretter en midlertidig mappe og returnerer banen til den. Dette kan være nyttig hvis du trenger å lagre flere midlertidige filer på samme sted.

## Se også

- Elixir sin offisielle dokumentasjon for `File` og `Dir` modulene: https://hexdocs.pm/elixir/File.html og https://hexdocs.pm/elixir/Dir.html
- En artikkel om hvordan du kan slette en midlertidig fil eller mappe ved å bruke Elixir: https://søtekode.com/no/article/elixir-sletting-av-midlertidig-fil-mappe.html