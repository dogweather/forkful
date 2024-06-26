---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:18.139022-07:00
description: "Hvordan: For \xE5 matche en streng mot et enkelt m\xF8nster, kan du\
  \ bruke `match`-metoden. Nedenfor sjekker vi om ordet \"Ruby\" finnes i en gitt\
  \ streng."
lastmod: '2024-03-13T22:44:41.305851-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 matche en streng mot et enkelt m\xF8nster, kan du bruke `match`-metoden."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:


### Grunnleggende matching
For å matche en streng mot et enkelt mønster, kan du bruke `match`-metoden. Nedenfor sjekker vi om ordet "Ruby" finnes i en gitt streng.

```ruby
if /Ruby/.match("Hei, Ruby!")
  puts "Match funnet!"
end
# Output: Match funnet!
```

### Mønstermatching med Variabler
Du kan interpolere variabler i regexen din ved å bruke `#{}`-syntaksen, noe som gjør mønstrene dine dynamiske.

```ruby
language = "Ruby"
if /#{language}/.match("Programmering i Ruby er gøy.")
  puts "Snakker om Ruby!"
end
# Output: Snakker om Ruby!
```

### Bruk av Regex til Erstatning
`gsub`-metoden lar deg erstatte hver forekomst av et mønster med en angitt erstatningsstreng.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Output: barbarbar
```

### Fangst
Parenteser i et regex brukes til å fange deler av et treff. `match`-metoden returnerer et `MatchData`-objekt, som du kan bruke til å få tilgang til fangster.

```ruby
match_data = /(\w+): (\d+)/.match("Alder: 30")
puts match_data[1] # Fangstet etikett
puts match_data[2] # Fangstet verdi
# Output:
# Alder
# 30
```

### Bruk av Tredjepartsbiblioteker
Selv om Rubys standardbibliotek er kraftfullt, kan du noen ganger trenge mer spesialisert funksjonalitet. Et populært gem for arbeid med regex er `Oniguruma`, som tilbyr ytterligere regex-funksjoner utover den innebygde Ruby regex-motoren.

Installer det ved å bruke:
```bash
gem install oniguruma
```

Eksempelbruk kan se ut som dette (forutsatt at du har krevd `oniguruma` etter installasjon):

```ruby
# Dette er et mer avansert eksempel og kan kreve ytterligere oppsett
require 'oniguruma'

mønster = Oniguruma::ORegexp.new('(\d+)')
match_data = mønster.match("Tallet er 42.")
puts match_data[1]
# Output: 42
```

Husk, selv om de er kraftfulle, kan regulære uttrykk bli komplekse og vanskelige å håndtere for mer kompliserte mønstre. Sikte på lesbarhet, og vurder alternative metoder hvis regexen din blir for innviklet.
