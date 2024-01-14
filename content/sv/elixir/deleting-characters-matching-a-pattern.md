---
title:                "Elixir: Radera tecken som matchar ett mönster"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
I Elixir-programmering kan det vara nödvändigt att ta bort tecken som matchar ett visst mönster, till exempel när man arbetar med strängar eller listor. Detta kan hjälpa till att rensa upp data eller förbereda det för vidare bearbetning. I denna bloggpost kommer vi att gå igenom hur man kan ta bort tecken som matchar ett visst mönster i Elixir-programmering.

## Hur man gör
För att ta bort tecken som matchar ett mönster behöver vi använda oss av funktionen `String.replace/4` i Elixir. Denna funktion tar in fyra argument: den ursprungliga strängen, det mönster vi vill matcha, det mönster vi vill byta ut de matchande tecknen med, och slutligen en flagga som talar om huruvida matchning ska ske globalt eller inte.

```
iex> String.replace("Hejsan på dig!", "a", "")
=> "Hjsn på dig!"
```

Vi kan även använda reguljära uttryck i mönstret som en mer avancerad form av matchning. Till exempel kan vi ta bort alla siffror från en sträng genom att använda reguljära uttrycken `[:digit:]` i vårt mönster.

```
iex> String.replace("Det finns 10 kor på gården.",~r/[:digit:]/,"")
=> "Det finns kor på gården."
```

## Djupdykning
Förutom att ta bort tecken som enkelt matchar ett visst mönster, kan vi också använda oss av uttryck för att göra mer komplexa substitutioner. Till exempel kan vi använda uttryck för att ändra på ordningen av tecken eller ta bort ord från en sträng.

En annan användbar funktion för att hantera strängar i Elixir är `String.split/2`, som delar upp en sträng baserat på ett visst separator-tecken eller mönster.

```
iex> String.split("hej,värld", ",")
=> ["hej", "värld"]
```

## Se även
- [Elixir dokumentation för String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Elixir dokumentation för reguljära uttryck](https://hexdocs.pm/elixir/Regex.html)
- [Elixir dokumentation för String.split/2](https://hexdocs.pm/elixir/String.html#split/2)