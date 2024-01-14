---
title:                "Elixir: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor: Å utføre feilsøking i dine Elixir programmer

Det kan være frustrerende å prøve å finne ut hvorfor et program ikke fungerer som forventet. En måte å løse dette på er å bruke debugging utskrift i Elixir. Dette hjelper deg med å spore verdier og flyten av programmet ditt for å identifisere feil. I denne bloggposten vil vi se på hvorfor og hvordan du kan bruke printing debug output i Elixir.

## Hvordan du kan gjøre det:

```Elixir
defmodule Person do
  def create(name, age) do
    # printing debug output før kall til `Person.new/2` 
    IO.inspect("Oppretter person med navn: #{name} og alder: #{age}")
    
    Person.new(name, age)
  end
end
```

I dette eksempelet ser vi på en `create` funksjon i en Person modul som tar inn to parametere: navn og alder. Før kallet til `Person.new/2`, skriver vi ut en melding ved hjelp av `IO.inspect/2` funksjonen. Dette vil skrive ut verdien av `name` og `age` variabelen til vår konsoll. Når vi kjører programmet, vil vi se utskriften vår og bekrefter at verdiene som sendes inn er riktige.

Output: `Oppretter person med navn: John og alder: 30`

Denne enkle utskriften kan hjelpe oss med å identifisere problemer eller uventede verdier i vårt program. Det er også verdt å merke seg at `IO.inspect/2` også fungerer med komplekse datastrukturer, noe som gjør det til et kraftig verktøy for debugging.

## Deep Dive

Nå som vi har sett hvordan vi kan bruke printing debug output, la oss se på noen avanserte funksjoner som Elixir tilbyr oss for debugging. Først har vi `IO.inspect/2` funksjonen, som vi allerede har brukt. Denne funksjonen tar inn en verdi og returnerer samme verdi, noe som gjør det enkelt å sette den inn i løpende kode uten å forstyrre flyten. 

En annen nyttig funksjon er `IO.puts/2` som printer en melding uten noen ekstra formatering. Dette er nyttig når du bare vil få en enkel utskrift uten verdier. Og hvis du vil ha mer kontroll over utskriften din, kan du bruke `Kernel.inspect/2` funksjonen som tar inn en verdi og et sett med opsjoner for å formatere utskriften.

## Se også

- [Debugging i Elixir](https://hexdocs.pm/elixir/debugging.html)
- [Elixir IO modulen](https://hexdocs.pm/elixir/IO.html)
- [Elixir Kernel modulen](https://hexdocs.pm/elixir/Kernel.html)