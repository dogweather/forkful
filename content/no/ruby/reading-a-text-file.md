---
title:                "Ruby: Å lese en tekstfil"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

 Det å kunne lese fra en tekstfil er en viktig ferdighet for enhver programmerer. Dette er fordi det tillater deg å interagere med store mengder data på en strukturert og effektiv måte. Enten du leser fra en loggfil eller en konfigurasjonsfil, vil evnen til å lese fra tekstfiler være svært nyttig.

# Hvordan gjøre det

Det finnes flere måter å lese fra en tekstfil i Ruby, men den vanligste er å bruke File-objektet. Først må du åpne tekstfilen ved hjelp av File.open() metoden og angi om du ønsker å lese fra filen eller skrive til den, ved å bruke "r" eller "w" parameteren.

```Ruby
File.open("tekstfil.txt", "r") do |fil|
  # kode for å lese fra filen
end
```
Inne i blokken kan du bruke `fil.readlines` metoden til å lese alle linjene i filen og lagre dem i en variabel. Deretter kan du bruke en `each` løkke for å iterere gjennom hver linje og gjøre ønskede manipulasjoner.

```Ruby
File.open("tekstfil.txt", "r") do |fil|
  linjer = fil.readlines
  linjer.each do |linje|
    # kode for å gjøre manipulasjoner på hver linje
  end
end
```
En annen måte å lese fra en tekstfil på er å bruke `File.foreach` metoden. Dette vil iterere gjennom hver linje i filen og utføre en gitt handling på hver linje.

```Ruby
File.foreach("tekstfil.txt") do |linje|
  # kode for å gjøre en handling for hver linje
end
```

# Dykk dypere

Når du leser fra en tekstfil, kan det være nyttig å forstå hvordan ruby håndterer linjeskift. Når du bruker `readlines` metoden, vil ruby automatisk fjerne linjeskiftet fra hver linje og lagre dem som separate elementer i en array. Men når du bruker `foreach` metoden, vil ruby behandle hver linje som en string, inkludert linjeskiftet.

En annen viktig ting å huske på er at ruby vil behandle alle tegn i en tekstfil som en string, selv tall og symboler. Derfor må du bruke metoder som `to_i` eller `to_f` for å konvertere strenger til tall.

# Se også

- [Ruby dokumentasjon for File-objektet](https://ruby-doc.org/core-2.7.2/File.html)
- [Tutorial: How to Read a Text File in Ruby](https://www.educative.io/edpresso/how-to-read-a-text-file-in-ruby)