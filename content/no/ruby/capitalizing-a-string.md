---
title:                "Ruby: Konvertering av streng til stor bokstav"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Du lurer kanskje på hvorfor noen ville ville ha behov for å kapitalisere en streng i Ruby. Å kapitalisere en streng betyr rett og slett å gjøre den første bokstaven stor, og deretter resten av bokstavene små. Dette kan være nyttig for estetiske årsaker, men også for å følge standarder i forhold til titler eller navngiving av variabler.

## Hvordan

Det er flere måter å kapitalisere en streng i Ruby.

```Ruby
"hei, verden".capitalize # => "Hei, verden" 
```

Vi kan bruke metoden `capitalize`, som returnerer strengen med bare den første bokstaven stor og resten små. 

```Ruby
"Hei, verden".upcase # => "HEI, VERDEN"
```

Vi kan også bruke metoden `upcase`, som gjør alle bokstavene store. Men merk at dette også vil endre eventuelle andre store bokstaver i strengen.

```Ruby
"Hei, Verden".downcase # => "hei, verden"
```

Metoden `downcase` vil gjøre alle bokstavene små.

Vi kan også bruke `capitalize!`, `upcase!` og `downcase!` for å endre strengen permanent, istedenfor å bare returnere en ny versjon av strengen.

## Dypdykk

Når vi bruker disse metodene, vil det som regel bare endre bokstavene som eksisterer i standard ASCII-tegnsettet. Dette betyr at bokstavene med aksenter eller andre spesielle tegn ikke vil endres.

For å kapitalisere en streng med alle bokstaver, inkludert de med aksenter og spesielle tegn, kan vi bruke metoden `mb_chars` fra biblioteket ActiveSupport.

```Ruby
require 'active_support/all'
"Hei, verden".mb_chars.capitalize # => "Hei, Verden"
```

## Se også

- [Ruby documentasjon om strenger](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby on Rails ActiveSupport dokumentasjon om mb_chars](https://api.rubyonrails.org/v6.1.4.1/classes/String.html#method-i-mb_chars)