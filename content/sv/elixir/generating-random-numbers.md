---
title:                "Elixir: Generera slumpmässiga nummer"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

I Elixir är möjligheterna för att generera slumpmässiga nummer många och kraftfulla. Om du behöver randomiserad data för testning, simulering eller någon annan applikation, är Elixir det perfekta språket för uppgiften.

## Hur man gör

För att generera ett slumpmässigt nummer i Elixir kan du använda funktionen `:rand.uniform/1`. Till exempel, om du vill generera ett nummer mellan 1 och 100 kan du använda koden nedan:

```Elixir
num = :rand.uniform(100)
IO.puts(num)
```

Detta kommer att returnera ett slumpmässigt heltal mellan 1 och 100, och sedan skriva ut det till terminalen. Om du vill ha ett annat intervall, såsom -10 till 10, kan du använda `:rand.uniform(-10..10)` istället.

För att generera en lista av slumpmässiga nummer, kan du använda funktionen `Enum.map/2` tillsammans med `:rand.uniform/1`. Se koden nedan:

```Elixir
random_nums = Enum.map(1..10, fn _ -> :rand.uniform(100) end)
IO.inspect(random_nums)
```

Detta kommer att skapa en lista med 10 slumpmässiga heltal mellan 1 och 100, och sedan skriva ut listan till terminalen. Du kan också använda `Enum.reduce/3` för att generera en enskild slumpmässig siffra baserat på en lista med startvärde.

## Djupdykning

Det finns många möjligheter inom Elixir för att generera slumpmässiga nummer. Du kan använda olika tillknytningsvärden för att kontrollera fördelningen av slumpmässiga nummer, använda olika algoritmer för att generera pseudo-slumpmässiga nummer och använda funktioner som `state` och `seed` för att skräddarsy dina resultat. Du kan också använda bibliotek som [erlang's `:rand`] (https://erlang.org/doc/man/rand.html) för att få tillgång till fler funktioner och möjligheter.

## Se även

- [Elixir Dokumentation för `:rand.uniform/1`] (https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3Arand.uniform%2F1)
- [Erlang Dokumentation för `:rand`] (https://erlang.org/doc/man/rand.html)
- [Blogginlägg om Slumpmässighet i Elixir] (https://medium.com/elixir-magic/generate-random-things-with-elixir-cab0c8d5b4f2)