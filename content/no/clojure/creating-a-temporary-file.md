---
title:    "Clojure: Opprette en midlertidig fil"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en vanlig oppgave i programmering, spesielt når man jobber med håndtering av data. Det kan være nyttig å midlertidig lagre data i en fil for senere å kunne bruke det i koden din. 

## Hvordan gjøre det i Clojure

Det er flere måter å opprette midlertidige filer i Clojure på, men den mest vanlige metoden er å bruke funksjonen `with-open`, som automatisk lukker filen etter at koden er utført. For eksempel:

```Clojure
(with-open [f (java.io.File/createTempFile "my-data-" ".txt")]
  (spit f "Dette er data som skal lagres midlertidig"))
```

Dette vil lage en midlertidig fil med navnet "my-data-" og legge til en unik ID og ".txt"-filtype. Deretter vil funksjonen `spit` skrive dataene til filen. Etter at koden er utført, vil filen automatisk lukkes og slettet fra systemet.

## Dypdykk

`with-open` funksjonen håndterer automatisk feil og stenger filen selv om det oppstår en feil i koden din. Dette er en veldig nyttig funksjon for å hindre lekkasjer og sikre at midlertidige filer ikke forblir åpne i systemet.

I tillegg til `createTempFile`-funksjonen, kan du også bruke `createTempDir` for å opprette en midlertidig mappe.

## Se også

- [Clojure Dokumentasjon](https://clojure.org/guides/destructuring#_with_open)
- [Java Fil Objekt](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Java IO Tutorial](https://www.tutorialspoint.com/java/io/index.htm)