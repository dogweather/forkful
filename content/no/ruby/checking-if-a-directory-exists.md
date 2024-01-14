---
title:    "Ruby: Kontrollere om en mappe eksisterer"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor
Å sjekke om en mappe eksisterer er en viktig del av programmering, spesielt når man skal håndtere filer og strukturere data. Det kan også være nødvendig når man vil sikre at man ikke overskriver eksisterende data eller for å sørge for at programmet fungerer som forventet.

## Hvordan
For å sjekke om en mappe eksisterer, kan man bruke `File.exist?` metoden i Ruby. Her er et enkelt eksempel:

```ruby
if File.exist?("mappe_navn")
  puts "Mappen eksisterer"
else
  puts "Mappen eksisterer ikke"
end
```

Dette vil sjekke om mappen med navnet "mappe_navn" eksisterer i det nåværende arbeidsområdet. Hvis mappen finnes, vil konsollen skrive ut "Mappen eksisterer", ellers skrives det ut "Mappen eksisterer ikke". 

Man kan også inkludere en absolutt sti til mappen, for eksempel `"/brukere/john/mappe_navn"` i stedet for bare mappenavnet.

## Dypdykk
Når man bruker `File.exist?` metoden, returneres det en boolean verdi, enten 'true' hvis mappen eksisterer eller 'false' hvis den ikke gjør det. Dette kan være nyttig for å kontrollere og styre programflyten basert på om en mappe eksisterer eller ikke.

Det finnes også andre metoder i Ruby for å jobbe med mapper, for eksempel `File.directory?` som returnerer 'true' hvis en gitt sti er en mappe, og `Dir.exists?` som sjekker om en mappe eksisterer.

## Se Også
- [Ruby dokumentasjon for File-klassen](https://ruby-doc.org/core-2.5.1/File.html)
- [Stack Overflow svar på hvordan sjekke om en mappe eksisterer i Ruby](https://stackoverflow.com/a/2677486)
- [Tutorial på å håndtere mapper i Ruby](https://www.rubyguides.com/2015/02/ruby-file/)