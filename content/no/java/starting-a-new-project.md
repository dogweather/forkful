---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Java"
category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt handler om å skape en ny kodebase for et spesifikt formål. Programutviklere gjør dette for å løse et problem, forbedre en prosess, eller lage en tjeneste.

## Hvordan:

Her viser vi hvordan man oppretter et nytt Java-prosjekt ved hjelp av et verktøy som IntelliJ IDEA.
```
Java
// Create a new Project
> File > New > Project...

// Select `Java`
> New Project > Java

// Define project location and name
> Project Location: D:\Projects\HelloWorld
> Project Name: HelloWorld

// Click Finish to complete
> Finish
```
Når prosjektet er opprettet, lager vi en hovedklasse med en main metode:
```
Java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```
Output blir da:
```
Java
Hello, World!
```

## Deep Dive:

Historisk sett har Java-prosjekter blitt startet manuelt ved å skrive koden fra bunnen av. I dag er det vanlig å bruke utviklerverktøy som IntelliJ IDEA, som håndterer mye av det grunnleggende oppsettet for deg.

Det er mange måter å starte et nytt prosjekt på. Alternativ til IntelliJ IDEA, er verktøy som Eclipse eller Apache NetBeans. Valg av verktøy er ofte bestemt av programmererens preferanse eller prosjektets spesifikke krav.

Et typisk Java-prosjekt har denne strukturen:
```
Java
HelloWorld
└───src
    └───Main.java
```
Dette er relativt enkelt for små prosjekter, men for større prosjekter kan det være lurt å bruke byggeverktøy som Maven eller Gradle for å håndtere avhengigheter og byggeprosesser.

## Se Også:

- IntelliJ IDEA: https://www.jetbrains.com/idea/
- Apache Maven: https://maven.apache.org/
- Gradle: https://gradle.org/