---
title:                "Interpolering av en streng"
html_title:           "Ruby: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å interpolere en streng i Ruby betyr å erstatte deler av en streng med variabler eller uttrykk. Dette gjør det enklere for utviklere å sette sammen setninger eller tekststrenger som inneholder variabel data. Det er en praktisk teknikk for å lage personlige meldinger eller dynamiske tilbakemeldinger i et program.

# Slik gjør du det:
```Ruby
# Her er et enkelt eksempel på interpolering av en streng
navn = "Heidi"
puts "Hei #{navn}, velkommen til Ruby-programmering!"

# Output:
Hei Heidi, velkommen til Ruby-programmering!
```

# Dypdykk:
Historisk sett har interpolering vært en del av Ruby siden starten, og det er også tilgjengelig i mange andre programmeringsspråk som Python og JavaScript. Alternativene til interpolering inkluderer å bruke string concatenation med «+» tegn, men dette kan bli upraktisk for større og mer komplekse strenger. I Ruby er det også mulig å konvertere andre datatyper til strenger mens du interpolerer, ved hjelp av metoden .to_s. 

# Se også:
- [Ruby String interpolation](https://www.rubyguides.com/2016/04/ruby-string-interpolation/)
- [String Interpolation in Ruby](https://dev.to/bimsan/string-interpolation-in-ruby-2hm)