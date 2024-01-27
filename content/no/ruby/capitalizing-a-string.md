---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å gjøre det første bokstaven i strengen stor (uppercase). Programmerere bruker dette for å formatere tekst etter grammatiske regler eller brukergrensesnittkrav.

## Hvordan gjøre det:
```Ruby
# Eksempel på å kapitalisere en streng
navn = "oslo"
puts navn.capitalize  # Output: Oslo

# Kapitalisering av alle ord i en streng
tittel = "velkommen til norge"
puts tittel.split.map(&:capitalize).join(' ')  # Output: Velkommen Til Norge
```

## Dypdykk
Kapitalisering av strenger i Ruby har gamle røtter, mye brukt i andre programmeringsspråk også. Ruby tilbyr metoder som `.capitalize` og `.titleize` (Rails) for å gjøre dette enkelt. Det er alternativer som `.upcase`, som gjør alle bokstavene store, eller `.downcase` for å gjøre dem små. Implementeringsdetaljer viktig å merke seg: `.capitalize` endrer bare den første bokstaven av hele strengen, mens en kjede av `.split.map` og `.capitalize` kombinerer flere metoder for å kapitalisere hvert ord individuelt.

## Se også
- Ruby dokumentasjon om strenger: [Ruby Docs](https://ruby-doc.org/core-3.1.0/String.html)
- Rails guide til inflections: [Rails Guides](https://guides.rubyonrails.org/active_support_core_extensions.html#inflections)
- Stack Overflow tråder om strengkapitalisering: [Stack Overflow](https://stackoverflow.com/questions/tagged/ruby+capitalize)
