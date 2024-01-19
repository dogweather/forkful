---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proste pobranie zawartości HTML danej strony. Programiści robią to w celu analizy, testowania, a czasem nawet scrapingu danych.

## Jak to zrobić:

```TypeScript
import axios from 'axios';

const getPokemons = async () => {
  try {
    const response = await axios.get('https://pokeapi.co/api/v2/pokemon/ditto');
    console.log(response.data);
  } catch (error) {
    console.error(error);
  }
}

getPokemons();
```

To jest nieskomplikowany kod TypeScript do pobierania strony API Pokemon za pomocą `axios`. Po uruchomieniu będziesz widział dane Pokemonu - 'Ditto'.

## Wgłębna analiza

Historia pobierania stron internetowych rozpoczęła się wraz z rozwojem internetu. Początkowo, było to stosowane do testowania i analizowania stron. Dziś jest to podstawa do scrapingu danych i automatyzacji sieci.

Jest wiele alternatyw do `axios`, takich jak `fetch`, `request` czy `superAgent`. Każda z nich ma swoje plusy i minusy, a wybór zależy od twoich specyficznych potrzeb.

Szczegółowa implementacja pobierania strony internetowej zależy od wielu czynników; np. jak skomplikowany jest twój cel, jakie dane chcesz wydobyć, czy chcesz to zautomatyzować itp.

## Zobacz też:

1. Axios - https://github.com/axios/axios
2. Fetch - https://developer.mozilla.org/pl/docs/Web/API/Fetch_API/Using_Fetch
3. Request - https://github.com/request/request
4. SuperAgent - https://github.com/visionmedia/superagent

Zawsze sprawdzaj i porównuj swoje opcje przed podjęciem decyzji. Zapoznaj się z powyższymi linkami i wybierz najbardziej efektywne narzędzie dla swoich potrzeb. Dobre programowanie!