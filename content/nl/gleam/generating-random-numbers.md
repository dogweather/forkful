---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:01:13.021989-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in programmeren kan cruciaal zijn voor het creëren van simulaties, testen, cryptografie en spellen. In Gleam is het een functie die ontwikkelaars in staat stelt onvoorspelbaarheid te introduceren of real-world scenario's in hun applicaties te simuleren.

## Hoe:

Om willekeurige getallen in Gleam te genereren, gebruik je voornamelijk de `gleam_random` bibliotheek. Deze bibliotheek biedt functies om willekeurige gehele getallen, drijvende komma's en meer te genereren. Zorg eerst dat je `gleam_random` hebt toegevoegd aan je `rebar.config` of `mix.exs` bestand als een afhankelijkheid.

Laten we enkele voorbeelden bekijken:

### Een Willekeurig Geheel Getal Genereren

Om een willekeurig geheel getal binnen een gespecificeerd bereik te produceren, kun je de `int` functie gebruiken:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Deze functie genereert een willekeurig geheel getal tussen 1 en 10 inclusief.

### Een Willekeurige Zwevende Komma Genereren

Om een willekeurige drijvende komma te krijgen, gebruik je de `float` functie. Dit genereert een drijvende komma tussen 0.0 en 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Voorbeelduitvoer

Het uitvoeren van deze functies kan uitvoeren zoals:

- Voor `generate_random_int()`: `5`
- Voor `generate_random_float()`: `0.84372`

Onthoud, elke uitvoering kan leiden tot verschillende uitvoeren vanwege de aard van willekeurigheid.

## Uitgebreide Verkenning

De `gleam_random` module implementeert een pseudotoevalsgenerator (PRNG), wat in wezen betekent dat de getallen niet echt willekeurig zijn, maar moeilijk te voorspellen zijn en daardoor willekeurigheid nabootsen. PRNG's werken door te starten met een initiële waarde, bekend als de zaadwaarde, en wiskundige bewerkingen toe te passen om een reeks getallen te genereren.

Historisch gezien hebben talen en bibliotheken verschillende algoritmen geïmplementeerd voor PRNG's, zoals de Mersenne Twister of Lineaire Congruentiegenerator (LCG). De keuze van het algoritme beïnvloedt de kwaliteit van de "willekeurigheid," waarbij sommige meer geschikt zijn voor cryptografische toepassingen dan andere. Hoewel de standaardbibliotheek van Gleam gemak en gebruiksvriendelijkheid biedt met zijn `gleam_random` module, is het niet altijd de beste keuze voor gebruikssituaties die cryptografisch veilige willekeurigheid vereisen. Voor cryptografische doeleinden zouden ontwikkelaars moeten kijken naar bibliotheken die specifiek zijn ontworpen om cryptografisch veilige pseudotoevalsgeneratoren (CSPRNG's) te bieden, die zijn ontworpen om aanvallen te weerstaan die toekomstige getallen zouden kunnen voorspellen op basis van het observeren van een reeks gegenereerde getallen.

Ter conclusie, hoewel de functionaliteit voor het genereren van willekeurige getallen van Gleam robuust is voor algemene programmeerbehoeften, moeten applicaties met specifieke beveiligingseisen overwegen om specifieke cryptografische oplossingen te gebruiken om de integriteit en beveiliging van hun willekeurige getalgeneratie te waarborgen.
