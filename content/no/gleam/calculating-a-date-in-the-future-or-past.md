---
title:    "Gleam: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor
Å beregne en dato i fremtiden eller fortiden er en viktig oppgave for mange programmerere. Det kan være nyttig for å tilpasse funksjonalitet basert på bestemte datoer, planlegge tidsavhengige oppgaver eller for å lage tidsrelaterte grafer og analyser. Å lære hvordan man kan gjøre dette i Gleam kan åpne opp for flere kreative løsninger og optimalisere arbeidsflyten din.

# Slik gjør du det
For å beregne en dato i fremtiden eller fortiden i Gleam, kan du bruke funksjonene `add_days`, `add_weeks`, `add_months` og `add_years` fra biblioteket `Time.Date`. Disse funksjonene tar inn en `Time.Date`-verdi og et heltall som angir antall dager, uker, måneder eller år du vil legge til eller trekke fra. Her er et eksempel på hvordan du kan bruke disse funksjonene:

```
Gleam
let initial_date = Time.Date.new(2021, 8, 10)
let date_in_10_days = Time.Date.add_days(initial_date, 10)
let date_in_2_weeks = Time.Date.add_weeks(initial_date, 2)
let date_in_3_months = Time.Date.add_months(initial_date, 3)
let date_in_5_years = Time.Date.add_years(initial_date, 5)

IO.println(date_in_10_days) // 2021-08-20
IO.println(date_in_2_weeks) // 2021-08-24
IO.println(date_in_3_months) // 2021-11-10
IO.println(date_in_5_years) // 2026-08-10
```

# Dykk dypere
Når du beregner en dato i fortiden eller fremtiden, må du ta hensyn til skuddår og tidszoner. I Gleam kan du bruke funksjonen `Time.Date.from_gregorian_date` for å lage en dato ved hjelp av en gregoriansk dato og tidsone. Dette kan være nyttig hvis du for eksempel arbeider med internasjonale datoer og behøver å konvertere til riktig tidsone før du beregner en dato.

En annen viktig ting å huske på er at en dato i Gleam er uavhengig av klokkeslett og tidssone, noe som betyr at du kun kan beregne datoer, ikke datoer og klokkeslett samtidig. Hvis du har behov for å beregne datoer og klokkeslett, kan du bruke funksjonene `Time.DateTime.from_gregorian_datetime` og `Time.DateTime.add_duration`.

# Se også
- [Gleam dokumentasjon for Time.Date](https://gleam.run/modules/gleam_time.html#date)
- [Tutorial: Beregne forskjellen mellom to datoer i Gleam](https://medium.com/@gleamlang/tutorial-calculating-the-difference-between-two-dates-in-gleam-6b6c651d1313)