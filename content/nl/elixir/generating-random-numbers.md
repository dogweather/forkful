---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:01:04.688043-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in Elixir is een fundamentele programmeertaak, essentieel voor applicaties die onvoorspelbare resultaten nodig hebben zoals bij het genereren van veilige tokens, het bemonsteren van gegevens, of in spelalgoritmen. Programmeurs gebruiken het om een niveau van willekeur en variabiliteit in hun applicaties te introduceren, waardoor ze dynamischer en minder deterministisch worden.

## Hoe te:

Om willekeurige getallen te genereren in Elixir, gebruik je voornamelijk de `:rand` module die verschillende functies biedt voor dit doel. Hier is een snelle gids om je te helpen beginnen:

Zorg eerst dat je de generator voor willekeurige getallen seedt om deze te initialiseren met een uniek startpunt:

```elixir
:rand.seed(:exsplus)
```

Om een willekeurig geheel getal binnen een bereik te genereren, gebruik je:

```elixir
random_integer = :rand.uniform(10) # Genereert een nummer tussen 1 en 10
IO.puts(random_integer)
```

Voor een willekeurige float tussen 0 en 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Je hebt mogelijk een specifieker bereik voor floats nodig, wat iets meer berekening vereist:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Onthoud, deze getallen zijn pseudo-willekeurig; ze worden bepaald door de seed en het algoritme maar zijn voldoende voor de meeste toepassingen.

## Diepgaande duik

De mogelijkheden van Elixir voor het genereren van willekeurige getallen steunen op de `:rand` module van Erlang, wat zijn erfgoed en nauwe relatie met Erlang weerspiegelt. De `:rand` module verving de oudere `:random` module, en biedt verbeterde algoritmen voor het genereren van willekeurige getallen. Het biedt een verscheidenheid aan algoritmen, waarbij `exsplus` de standaard is, maar ondersteunt ook andere zoals `exs64`, `exsl`, en meer, elk met zijn afwegingen wat betreft snelheid en kwaliteit van willekeurigheid.

Een interessant aspect van Elixir's (en dus Erlang's) willekeurige nummergeneratie is de behandeling van seeds. Het systeem handhaaft afzonderlijke seedstaten voor elk proces, om ervoor te zorgen dat gelijktijdige processen elkaar niet beïnvloeden in hun reeksen van willekeurige getallen. Dit is bijzonder nuttig in gelijktijdige applicaties, en zorgt voor voorspelbaarheid en betrouwbaarheid in gedistribueerde systemen.

Hoewel de `:rand` module voor de meeste toepassingen voldoende is, moeten applicaties die cryptografisch veilige willekeurige getallen vereisen andere opties overwegen. De `crypto` module biedt functies zoals `crypto:strong_rand_bytes/1` die zijn ontworpen om veilige willekeurige gegevens te genereren geschikt voor cryptografische doeleinden. Deze alternatieven zijn essentieel voor beveiligingsgevoelige applicaties zoals token generatie, encryptie en bepaalde typen authenticatiemechanismen.
