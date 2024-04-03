---
date: 2024-01-26 00:58:51.828169-07:00
description: "Hur: Ruby anv\xE4nder `begin`, `rescue`, `ensure` och `end` f\xF6r att\
  \ hantera fel. Du omsluter den riskfyllda koden med `begin` och `end`. Om ett fel\
  \ intr\xE4ffar\u2026"
lastmod: '2024-03-13T22:44:38.438442-06:00'
model: gpt-4-1106-preview
summary: "Ruby anv\xE4nder `begin`, `rescue`, `ensure` och `end` f\xF6r att hantera\
  \ fel."
title: Hantering av fel
weight: 16
---

## Hur:
Ruby använder `begin`, `rescue`, `ensure` och `end` för att hantera fel. Du omsluter den riskfyllda koden med `begin` och `end`. Om ett fel inträffar tar `rescue` vid.

```Ruby
begin
  # Riskfylld kod går här.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Hoppsan! Det där kan du inte göra: #{e.message}"
ensure
  puts "Det här körs alltid, fel eller inte."
end
```

Exempelutdata:
```
Hoppsan! Det där kan du inte göra: dividerat med 0
Det här körs alltid, fel eller inte.
```

## Fördjupning
Historiskt har felhantering i programmeringsspråk utvecklats avsevärt, där tidiga språk ofta hade grova eller obefintliga mekanismer. Rubys undantagshantering är inspirerad av språk som Python och Smalltalk.

Alternativ till `begin-rescue` i Ruby inkluderar att använda `rescue` i metoddefinitioner eller att använda `throw` och `catch` för icke-standardiserad flödeskontroll, även om de inte används för typisk felhantering.

En intressant detalj: Rubys undantag är objekt (instanser av klassen `Exception` och dess ättlingar), så du kan definiera anpassade felklasser och göra mer än att bara logga fel — du kan bära med dig rik status runt programmet för mer robust felhantering.

## Se även
- Ruby-dokumentationen om undantag och felhantering: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- En detaljerad guide om bästa praxis för felhantering i Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
