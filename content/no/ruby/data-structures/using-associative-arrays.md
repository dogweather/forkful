---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:43.911652-07:00
description: "Hvordan: \xC5 opprette og bruke hashes i Ruby er enkelt. Du kan initialisere\
  \ en tom hash, fylle den med n\xF8kkel-verdipar, f\xE5 tilgang til verdier ved hjelp\
  \ av\u2026"
lastmod: '2024-03-13T22:44:41.308771-06:00'
model: gpt-4-0125-preview
summary: "\xC5 opprette og bruke hashes i Ruby er enkelt."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
Å opprette og bruke hashes i Ruby er enkelt. Du kan initialisere en tom hash, fylle den med nøkkel-verdipar, få tilgang til verdier ved hjelp av nøklene deres, og mer. Slik gjør du det:

```Ruby
# Opprette en hash
my_hash = { "name" => "John Doe", "age" => 30 }

# En annen måte å opprette en hash på
another_hash = Hash.new
another_hash["position"] = "Utvikler"

# Få tilgang til hash-verdier
puts my_hash["name"] # Utdata: John Doe

# Legge til et nytt nøkkel-verdi-par
my_hash["language"] = "Ruby"
puts my_hash # Utdata: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Iterere gjennom en hash
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Utdata:
# name: John Doe
# age: 30
# language: Ruby
```

Du kan også bruke symboler som mer effektive nøkler:

```Ruby
# Bruke symboler for nøkler
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Utdata: Jane Doe
```

## Dypdykk:
Konseptet med assosiative tabeller er ikke unikt for Ruby; mange språk implementerer dem under ulike navn, som ordbøker i Python eller objekter i JavaScript (når de brukes som nøkkel-verdipar). I Ruby's tidlige stadier var hashes noe tregere og ikke så fleksible. Imidlertid, over tid, har Rubys implementering av hashes blitt svært optimalisert, spesielt for symbolnøkler, noe som gjør dem ekstremt effektive for hyppig tilgang og oppdateringer.

Rubys hashes utmerker seg med sin syntaktiske enkelhet og fleksibilitet - du kan bruke nesten hvilken som helst objekttype som nøkkel, selv om symboler og strenger er mest vanlige. Internt er Ruby-hashes implementert ved hjelp av en hashalgoritme som balanserer hastighet og minneeffektivitet, selv når antallet elementer skalerer opp.

Selv om hashes er utrolig fleksible, er de ikke den eneste løsningen for datalagring i Ruby. For ordnede samlinger er tabeller mer passende, og for sett med unike gjenstander kan et Sett være et bedre valg. I tillegg, for veldig komplekse datastrukturer, kan det være tilrådelig å opprette tilpassede klasser.

Husk at valget om å bruke en hash versus andre datastrukturer i stor grad kommer an på det spesifikke bruksområdet - hasher er utmerkede for raske oppslag og å opprettholde forbindelser mellom unike nøkler og deres verdier.
