---
title:                "Fouten afhandelen"
date:                  2024-01-28T22:01:36.860258-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling gaat over het verwachten van het onverwachte in code - fouten en problemen gracieus beheren zonder te crashen. Programmeurs doen dit om de stroom te controleren wanneer er iets misgaat en om de gebruikerservaring soepel te houden.

## Hoe te:

Ruby gebruikt `begin`, `rescue`, `ensure` en `end` om fouten te behandelen. Je wikkelt de risicovolle code in `begin` en `end`. Als er een fout optreedt, komt `rescue` in actie.

```Ruby
begin
  # Risicovolle code gaat hier.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Oeps! Dat kun je niet doen: #{e.message}"
ensure
  puts "Dit wordt altijd uitgevoerd, fout of niet."
end
```

Voorbeelduitvoer:
```
Oeps! Dat kun je niet doen: gedeeld door 0
Dit wordt altijd uitgevoerd, fout of niet.
```

## Diepgaand

Historisch gezien heeft de foutafhandeling in programmeertalen aanzienlijk geëvolueerd, waarbij vroege talen vaak ruwe of niet-bestaande mechanismen hadden. Ruby's uitzonderingsafhandeling is geïnspireerd door talen zoals Python en Smalltalk.

Alternatieven voor `begin-rescue` in Ruby omvatten het gebruik van `rescue` in methode-definities of het inzetten van `throw` en `catch` voor niet-standaard stroombeheersing, hoewel ze niet worden gebruikt voor typische foutafhandeling.

Een interessant detail: Ruby's uitzonderingen zijn objecten (instanties van de `Exception` klasse en haar nakomelingen), dus je kunt aangepaste foutklassen definiëren en meer doen dan alleen fouten loggen - je kunt een rijke staat door het programma dragen voor robuustere foutafhandeling.

## Zie Ook

- De Ruby documentatie over uitzonderingen en foutafhandeling: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Een gedetailleerde gids over de beste praktijken voor Ruby foutafhandeling: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
