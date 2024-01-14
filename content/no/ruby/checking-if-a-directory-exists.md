---
title:    "Ruby: Sjekke om en mappe eksisterer."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig for en Ruby-programmerer å vite hvordan man sjekker om en mappe eksisterer. Denne kunnskapen kan hjelpe deg med å lage en mer pålitelig og robust programvare og unngå eventuelle feil som kan oppstå når mappen ikke eksisterer. Derfor er det viktig å ha denne ferdigheten i verktøykassen din.

## Hvordan

Det er flere måter å sjekke om en mappe eksisterer i Ruby-programmering. En måte er å bruke `Dir.exist?` metoden, som vil returnere `true` hvis mappen eksisterer og `false` hvis den ikke gjør det.

```Ruby
if Dir.exist?("mappe_navn")
  puts "Mappen eksisterer."
else
  puts "Mappen eksisterer ikke."
end
```

En annen måte å sjekke på er å bruke `File.directory?` metoden, som også vil returnere `true` eller `false` basert på om mappen eksisterer eller ikke.

```Ruby
if File.directory?("mappe_navn")
  puts "Mappen eksisterer."
else
  puts "Mappen eksisterer ikke."
end
```

En tredje måte å gjøre dette på er ved å bruke `Dir.glob` metoden, som vil returnere en liste med filer og mapper som matcher angitt mønster. Hvis mappen eksisterer, vil mønsteret være lik mappens navn, og dermed vil det returnere et element i listen. Hvis mappen ikke eksisterer, vil listen være tom.

```Ruby
mappenavn = "mappe_navn"

if Dir.glob(mappenavn) == [mappenavn]
  puts "Mappen eksisterer."
else
  puts "Mappen eksisterer ikke."
end
```

I alle disse eksemplene kan du også angi den absolutte banen til mappen i stedet for bare navnet, hvis det er nødvendig.

## Dypdykk

Hvis du vil unngå å få en feilmelding når du prøver å få tilgang til en mappe som ikke eksisterer, kan du også bruke `Dir.mkdir` metoden til å opprette mappen hvis den ikke eksisterer. Hvis mappen allerede eksisterer, vil metoden bare returnere `nil`.

```Ruby
mappenavn = "mappe_navn"

Dir.mkdir(mappenavn) unless File.directory?(mappenavn) 
# hvis mappenavn allerede eksisterer, vil den ikke bli opprettet igjen
```

Det kan også være nyttig å sjekke om en mappe eksisterer før du prøver å skrive filer til den, for å unngå feil og sikre at filskrivingen er vellykket.

## Se også

- [Ruby dokumentasjon for Dir-klassen](https://ruby-doc.org/core/Dir.html)
- [Ruby dokumentasjon for File-klassen](https://ruby-doc.org/core/File.html)
- [Ruby globbing tutorial](https://www.rubyguides.com/2018/10/ruby-glob/)