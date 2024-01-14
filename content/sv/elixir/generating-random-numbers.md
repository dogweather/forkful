---
title:    "Elixir: Generera slumpmässiga nummer"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av de flesta programmeringsprojekt. Det kan användas för att skapa unika lösenord, simulera slumpmässiga händelser eller välja slumpmässiga värden från en lista. I denna bloggpost kommer vi att utforska hur man genererar slumpmässiga nummer med Elixir.

## Så här gör du

För att generera slumpmässiga nummer i Elixir finns det flera olika sätt att göra det. Ett sätt är att använda funktionen `:rand.uniform/0`, som genererar ett slumpmässigt flyttal mellan 0 och 1.

```Elixir
:rand.uniform() #=> 0.5869037038244131
```

Om du vill ha ett heltal istället kan du använda `:rand.uniform/1` och ge det ett intervall som argument.

```Elixir
:rand.uniform(10) #=> 7
:rand.uniform(1..100) #=> 45
```

Du kan också använda funktionen `:rand.seed/1` för att sätta en startpunkt för de slumpmässiga numrerna. Det innebär att du kan generera samma sekvens av slumpmässiga nummer varje gång med samma seed-värde.

```Elixir
:rand.seed(1234)
:rand.uniform() #=> 0.5347535595413858
:rand.uniform() #=> 0.4525331979163501
:rand.uniform() #=> 0.0647825241766279

:rand.seed(1234)
:rand.uniform() #=> 0.5347535595413858
:rand.uniform() #=> 0.4525331979163501
:rand.uniform() #=> 0.0647825241766279
```

Om du behöver generera en lista med slumpmässiga nummer kan du använda funktionen `:rand.uniform/2` för att specificera antalet nummer du vill ha.

```Elixir
:rand.uniform(10, 5) #=> [3, 7, 2, 9, 4]
```

## Djupdykning

Elixir använder en algoritm som kallas Mersenne Twister för att generera sina slumpmässiga nummer. Detta är en välkänd och robust algoritm som är känd för att ge lång period och god fördelning av sina slumpmässiga tal.

En intressant egenskap hos Mersenne Twister är att den kan ha flera olika seed-värden, vilket möjliggör att flera oberoende sekvenser av slumpmässiga nummer kan genereras samtidigt.

Slumpmässiga nummer i Elixir är dock inte helt slumpmässiga eftersom de genereras från en seed-värde. Om du behöver äkta slumpmässiga nummer kan du använda externt genererade seed-värden, som t.ex. från en fysisk slumpgenerator.

## Se även

- [Elixir Official Documentation on Random Numbers](https://hexdocs.pm/elixir/Kernel.html#rand/1)
- [Mersenne Twister Algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)