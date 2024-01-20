---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Interpolering av en sträng i Elixir innebär att du direkt införlivar variabler eller uttryck i en sträng. Programmerare gör det för att bygga dynamiska strängar mer lätt och tydlig.

## Hur gör man:
Här är ett enkelt exempel på interpolering. Skapa en variabel med värde och använd det sedan inom en sträng.
```Elixir
name = "Anna"
IO.puts "Hej #{name}!" 
```
Det här ger utskriften:
```
Hej Anna!
```
Du kan också utföra uttryck inom markörerna.
```Elixir
a = 10
b = 20
IO.puts "Summan av a och b är #{a + b}."
```
Utskriften blir:
```
Summan av a och b är 30.
```

## Djup Dykning
Elixir som följer sin föregångare Erlang för dess sträng hantering, stödjer sträng interpolering sedan början. Alternativ till interpolering inkluderar strängkonkatenering eller användning av funktionsformat, men interpolering erbjuder en renare och mer läsbar syntax. Vid implementering av sträng interpolering sker processen i två steg: 
1) kompilering och 
2) runtime-beräkning av uttrycket i interpoleringen.

## Se Även
Om du letar efter mer detaljerad information om stränginterpolation i Elixir, kan dessa källor vara till hjälp:

- Elixir Officiella dokumentation: [String interpolations](https://hexdocs.pm/elixir/String.html#module-interpolation)

Vänligen notera att dessa källor är på engelska.