---
title:                "Java: Kontrollere om en mappe eksisterer"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

I Java-programmering, er det viktig å sjekke om en mappe eksisterer før du prøver å gjøre noe med den. Dette kan forhindre unødvendige feil og problemer senere i koden din.

## Hvordan

Sjekke om en mappe eksisterer i Java er en ganske enkel prosess. Du trenger en File-objekt og bruker metoden `exists()` for å sjekke om mappen eksisterer. Her er et eksempel på hvordan du kan gjøre det:

```Java
File mappe = new File("minMappe");
if(mappe.exists()) {
    System.out.println("Mappen eksisterer.");
} else {
    System.out.println("Mappen eksisterer ikke.");
}
```

Hvis mappen eksisterer, vil utgangen være "Mappen eksisterer." Hvis den ikke eksisterer, vil utgangen være "Mappen eksisterer ikke."

## Dypdykk

Når du bruker `exists()`-metoden, vil den også sjekke om det er en fil med samme navn som mappen. Dette betyr at hvis du prøver å opprette en ny mappe med navnet "minMappe", vil `exists()` returnere true fordi en fil med samme navn allerede eksisterer. For å unngå dette, kan du bruke `isDirectory()`-metoden i tillegg til `exists()`. Dette vil sikre at det faktisk er en mappe med navnet du søker etter. Her er et eksempel på hvordan du kan gjøre det:

```Java
File mappe = new File("minMappe");
if(mappe.exists() && mappe.isDirectory()) {
    System.out.println("Mappen eksisterer.");
} else {
    System.out.println("Mappen eksisterer ikke.");
}
```

Dette vil gi deg et mer nøyaktig resultat når du sjekker om en mappe eksisterer eller ikke.

## Se også

- [Java Dokumentasjon - File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java Tutorials - Working with Files and Directories](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)