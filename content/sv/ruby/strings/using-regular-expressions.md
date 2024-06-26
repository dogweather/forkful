---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:05.348433-07:00
description: "Hur man g\xF6r: F\xF6r att matcha en str\xE4ng mot ett enkelt m\xF6\
  nster kan du anv\xE4nda metoden `match`. Nedan kontrollerar vi om ordet \"Ruby\"\
  \ finns i en given str\xE4ng."
lastmod: '2024-03-13T22:44:38.418656-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att matcha en str\xE4ng mot ett enkelt m\xF6nster kan du anv\xE4\
  nda metoden `match`."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:


### Grundläggande Matchning
För att matcha en sträng mot ett enkelt mönster kan du använda metoden `match`. Nedan kontrollerar vi om ordet "Ruby" finns i en given sträng.

```ruby
if /Ruby/.match("Hej, Ruby!")
  puts "Matchning hittad!"
end
# Utmatning: Matchning hittad!
```

### Mönstermatchning med Variabler
Du kan interpolera variabler i ditt regex med `#{}`-syntaxen, vilket gör dina mönster dynamiska.

```ruby
språk = "Ruby"
if /#{språk}/.match("Att programmera i Ruby är roligt.")
  puts "Pratar om Ruby!"
end
# Utmatning: Pratar om Ruby!
```

### Använda Regex för Substitution
Metoden `gsub` låter dig ersätta varje förekomst av ett mönster med en specificerad ersättningssträng.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Utmatning: barbarbar
```

### Fånga
Parenteser i ett regex används för att fånga delar av en matchning. Metoden `match` returnerar ett `MatchData`-objekt, som du kan använda för att komma åt fångster.

```ruby
match_data = /(\w+): (\d+)/.match("Ålder: 30")
puts match_data[1] # Fångad etikett
puts match_data[2] # Fångat värde
# Utmatning:
# Ålder
# 30
```

### Använda Tredjepartsbibliotek
Även om Rubys standardbibliotek är kraftfullt kan du ibland behöva mer specialiserad funktionalitet. Ett populärt gem för att arbeta med regex är `Oniguruma`, som erbjuder ytterligare regexfunktioner utöver Rubys inbyggda regexmotor.

Installera det med:
```bash
gem install oniguruma
```

Ett exempel på användning kan se ut så här (förutsatt att du har krävt `oniguruma` efter att ha installerat det):

```ruby
# Detta är ett mer avancerat exempel och kan kräva ytterligare inställningar
require 'oniguruma'

mönster = Oniguruma::ORegexp.new('(\d+)')
match_data = mönster.match("Numret är 42.")
puts match_data[1]
# Utmatning: 42
```

Kom ihåg, även om kraftfulla kan reguljära uttryck bli komplexa och svårhanterliga för mer komplicerade mönster. Sträva efter läsbarhet och överväg alternativa metoder om ditt regex blir för invecklat.
