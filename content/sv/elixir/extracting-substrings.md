---
title:    "Elixir: Extrahering av delsträngar"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är ett användbart verktyg inom Elixir-programmering. Det kan hjälpa dig att enkelt bearbeta och manipulera textsträngar för att lösa olika problem.

## Hur man gör det

Först och främst måste du ha en textsträng som du vill extrahera en del av. För att göra detta använder vi funktionen `String.slice/3`. Denna funktion tar in tre argument: den ursprungliga strängen, startindex och slutindex för den del du vill extrahera. Till exempel om vi bara vill ha de första tre tecknen ur strängen "Hejsan!", kan vi skriva:

```Elixir
String.slice("Hejsan!", 0, 3)
```

Detta returnerar "Hej" som output. Notera att index i Elixir börjar på 0, så "H" har index 0, "e" har index 1 och så vidare.

Vi kan också använda oss av funktionen `String.split/2` för att dela upp en sträng baserat på ett visst tecken eller mönster. Till exempel kan vi dela upp strängen "Hej, detta är en Elixir-bloggpost" baserat på kommatecknet, genom att skriva:

```Elixir
String.split("Hej, detta är en Elixir-bloggpost", ",")
```

Detta returnerar en lista med två strängar: "Hej" och "detta är en Elixir-bloggpost".

## Deep Dive

Det finns många andra användbara funktioner för att extrahera substrängar i Elixir, såsom `String.replace/3` för att ersätta delar av en sträng med något annat och `String.trim/1` för att ta bort onödiga mellanslag från början och slutet av en sträng.

Något att tänka på är att vanliga strängoperationer i Elixir returnerar alltid en ny sträng, istället för att modifiera den ursprungliga strängen. Detta säkerställer att vårt program är robust och inte riskerar att ändra på oförutsägbara sätt.

## Se även

- [Elixir Dokumentation: String](https://hexdocs.pm/elixir/String.html)
- [Elixir Skolan: Strängmanipulering](https://elixir-skolan.org/grundlaeggande-begrepp/strangmanipulering.html)
- [Elixir Forum: Substring Extraction](https://elixirforum.com/t/substring-extraction/1689)