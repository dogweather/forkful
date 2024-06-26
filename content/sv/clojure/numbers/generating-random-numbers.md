---
date: 2024-01-27 20:32:59.652338-07:00
description: "Hur man g\xF6r: I Clojure \xE4r generering av slumpm\xE4ssiga tal enkel,\
  \ och det finns ett par inbyggda funktioner som kan anv\xE4ndas direkt. F\xF6r att\
  \ generera ett\u2026"
lastmod: '2024-03-13T22:44:37.521309-06:00'
model: gpt-4-0125-preview
summary: "I Clojure \xE4r generering av slumpm\xE4ssiga tal enkel, och det finns ett\
  \ par inbyggda funktioner som kan anv\xE4ndas direkt."
title: Generera slumptal
weight: 12
---

## Hur man gör:
I Clojure är generering av slumpmässiga tal enkel, och det finns ett par inbyggda funktioner som kan användas direkt.

För att generera ett slumpmässigt flyttal mellan 0 (inklusive) och 1 (exklusive) kan du använda funktionen `rand`:

```Clojure
(rand)
;; Exempelutmatning: 0.7094245047062917
```

Om du behöver ett heltal inom ett specifikt intervall, använd `rand-int`:

```Clojure
(rand-int 10)
;; Exempelutmatning: 7
```

Detta ger dig ett slumpmässigt heltal mellan 0 (inklusive) och det tal du skickar som argument (exklusive).

För att generera ett slumpmässigt tal inom ett specifikt intervall (inte begränsat till heltal) kan du kombinera `rand` med aritmetik:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Användning
(rand-range 10 20)
;; Exempelutmatning: 14.857457734992847
```

Denna funktion `rand-range` kommer att returnera ett slumpmässigt flyttal mellan de `min` och `max` värden du specificerar.

För scenarion som kräver mer komplexa distributioner eller sekvenser av slumpmässiga tal där upprepbarhet är nödvändig (användning av frön), kanske du behöver titta på ytterligare bibliotek som sträcker sig bortom det som är inbyggt.

## Fördjupning
Den underliggande mekanismen för att generera slumpmässiga tal i de flesta programmeringsspråk, inklusive Clojure, är beroende av en pseudoslumpmässig talgenerator (PRNG). En PRNG använder en algoritm för att producera en sekvens av tal som approximerar egenskaperna hos slumpmässiga tal. Det är värt att notera att eftersom dessa genereras algoritmiskt, är de inte verkligt slumpmässiga men kan vara tillräckliga för de flesta praktiska ändamål.

I datorernas barndom var det en betydande utmaning att generera högkvalitativa slumpmässiga tal, vilket ledde till utvecklingen av olika algoritmer för att förbättra slumpmässighet och distribution. För Clojure är de inbyggda funktionerna, såsom `rand` och `rand-int`, praktiska för dagligt bruk och täcker ett brett spektrum av vanliga användningsfall.

Dock, för applikationer som kräver kryptografisk säkerhet eller mer komplexa statistiska provtagningsmetoder, vänder sig Clojure-utvecklare ofta till externa bibliotek som erbjuder mer robusta och specialiserade PRNGs. Bibliotek såsom `clj-random` ger tillgång till ett bredare utbud av algoritmer och större kontroll över seedning, vilket kan vara avgörande för simuleringar, kryptografiska applikationer eller något område där kvaliteten och förutsägbarheten i sekvensen av slumpmässiga tal kan ha betydande konsekvenser.

Medan Clojures inbyggda funktioner för att generera slumpmässiga tal är tillräckliga för många uppgifter, kan utforskandet av externa bibliotek erbjuda djupare insikter och alternativ för skräddarsydda eller mer kritiska applikationer.
