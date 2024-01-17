---
title:                "Genererande slumpmässiga tal"
html_title:           "Elixir: Genererande slumpmässiga tal"
simple_title:         "Genererande slumpmässiga tal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är en viktig del av programmering, eftersom det tillåter oss att skapa variation och dynamik i våra program. Genom att använda slumpen i våra program kan vi skapa spel, simuleringar och mer.

## Hur man gör:
Elixir har ett inbyggt bibliotek för att generera slumpmässiga tal, kallat :random. Här är ett exempel på hur du använder det:

```Elixir
# Generera ett slumpmässigt heltal mellan 1 och 10
:random.uniform(1, 10)

# Generera ett slumpmässigt decimaltal mellan 0 och 1
:random.uniform()

# Generera ett slumpmässigt tal från en lista
:random.sample([1, 2, 3, 4])

# Generera ett slumpmässigt tecken från en sträng
:random.sample("elixir")

```

Output:

```Elixir
8
0.532453234
3
"i"
```

## Djupdykning:
Att generera slumpmässiga tal har varit en utmaning för programmerare sedan lång tid tillbaka. Det finns olika algoritmer och metoder för att göra detta, men i Elixir använder vi standard metoden som kallas "Mersenne-Twister". Detta är en vanlig algoritm som ger en hög grad av slumpmässighet.

Det finns också andra bibliotek för att generera slumpmässiga tal i Elixir, som till exempel SecureRandom från OpenSSL. Detta bibliotek erbjuder också möjligheten att generera kryptografiskt säkra slumpmässiga tal.

Implementeringen av slumpmässiga tal i Elixir är snabb och effektiv tack vare den funktionella naturen hos språket. Detta gör det enkelt att använda i våra program, utan att behöva oroa oss för prestanda.

## Se även:
Om du vill lära dig mer om hur du använder slumpmässiga tal i Elixir, kolla in dessa resurser:

- Elixir :random Dokumentation: https://hexdocs.pm/elixir/Random.html
- SecureRandom: https://hexdocs.pm/elixir/SecureRandom.html