---
title:                "Elixir: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver. Enten du lager et spill, et statistikkverktøy eller en tilfeldig passordgenerator, trenger du ofte tilfeldige verdier for å gjøre programmet ditt mer dynamisk og interessant.

## Hvordan
For å generere tilfeldige tall i Elixir kan du bruke funksjonen `:rand.uniform/1`. Denne funksjonen tar inn et heltall som argument, og returnerer et tilfeldig tall mellom 0 og det gitte tallet (ikke inkludert).

```Elixir
:rand.uniform(10) # Output: 5
:rand.uniform(100) # Output: 77
```

Du kan også generere tilfeldige flyttall med `:rand.uniform/0`, som returnerer et tall mellom 0 og 1.

```Elixir
:rand.uniform() # Output: 0.5243899131402998
:rand.uniform() # Output: 0.19642607551113634
```

For å generere tilfeldige tall innenfor et gitt område, kan du bruke funksjonen `:rand.uniform/2`. Denne funksjonen tar to argumenter - det første er starten på området, og det andre er slutten på området. Den returnerer et tilfeldig tall mellom disse to verdiene.

```Elixir
:rand.uniform(20, 40) # Output: 34
:rand.uniform(10, 50) # Output: 29
```

## Dypdykk
Under overflaten, bruker Elixir `:rand.uniform/1` funksjonen faktisk `:rand.uniform/2` internt, med start på 0 og det gitte tallet som slutten på området. Dette betyr at den første varianten er mer effektiv, og bør brukes når det er mulig.

Elixir har også en `:rand.seed/1` funksjon som tar et heltall som argument, og lar deg sette en startverdi for tilfeldighetsgeneratoren. Dette er nyttig hvis du vil ha en forutsigbar sekvens av tilfeldige tall, eller hvis du vil kunne gjenskape en spesiell sekvens senere.

## Se også
- [Elixir Offisiell Dokumentasjon om tilfeldige tall](https://hexdocs.pm/elixir/1.12/Random.html)
- [Elixir School om tilfeldige tall i Elixir](https://elixirschool.com/en/lessons/advanced/random/)
- [Elixir Forum tråd om tilfeldige tall i Elixir](https://elixirforum.com/t/what-are-you-using-to-generate-random-numbers/36010)