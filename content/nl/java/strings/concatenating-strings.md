---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:10.553545-07:00
description: "Het samenvoegen van strings betekent dat je ze achter elkaar plakt om\
  \ een nieuwe string te maken. Het is handig voor het cre\xEBren van aangepaste berichten,\u2026"
lastmod: 2024-02-19 22:05:09.724453
model: gpt-4-0125-preview
summary: "Het samenvoegen van strings betekent dat je ze achter elkaar plakt om een\
  \ nieuwe string te maken. Het is handig voor het cre\xEBren van aangepaste berichten,\u2026"
title: Samenvoegen van strings
---

{{< edit_this_page >}}

## Wat & Waarom?

Het samenvoegen van strings betekent dat je ze achter elkaar plakt om een nieuwe string te maken. Het is handig voor het creëren van aangepaste berichten, het opbouwen van tekst voor output of het verwerken van gebruikersinput.

## Hoe:

Hier is de snelle uitleg over hoe je strings kunt samenvoegen in Java:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String firstName = "John";
        String lastName = "Doe";
        
        // Met de plus-operator
        String fullName = firstName + " " + lastName;
        System.out.println(fullName); // Output: John Doe
        
        // Met de concat()-methode
        String anotherFullName = firstName.concat(" ").concat(lastName);
        System.out.println(anotherFullName); // Output: John Doe
        
        // Met StringBuilder voor meerdere samenvoegingen
        StringBuilder builder = new StringBuilder();
        builder.append(firstName).append(" ").append(lastName);
        System.out.println(builder.toString()); // Output: John Doe
    }
}
```

## Diepere duik

Het samenvoegen van strings lijkt simpel genoeg, toch? Het zit al vanaf het begin in Java en we hebben een paar manieren om het te doen. Eerdere Java-versies gebruikten StringBuilder onder de motorkap wanneer je een simpele `+` deed. Toen kwam Java 5, en dingen werden efficiënter met de introductie van de `StringJoiner` en meer verbeteringen aan de `StringBuilder` klasse.

Nu vraag je je misschien af waarom niet altijd de `+` operator gebruiken als het hetzelfde is? Blijkt dat `+` geweldig is voor een snelle klus met kleine strings of een paar samenvoegingen. Achter de schermen kan het echter qua prestaties kostbaar worden als je het in een lus met veel iteraties gebruikt omdat het tijdelijke objecten creëert voordat de uiteindelijke stringversie bereikt wordt.

In die zware gevallen, gebruik je `StringBuilder` of `StringBuffer`. `StringBuilder` is doorgaans sneller omdat het niet gesynchroniseerd is - waardoor het thread-unsafe maar snel is. `StringBuffer` is de oudere, thread-safe optie. Het is trager vanwege de synchronisatie-overhead. Kies op basis van je behoeften aan thread safety.

Voor de `concat()` methode, het is eenvoudig maar niet zo flexibel als `StringBuilder`. Wil je in een lus blijven en meer strings toevoegen? `concat()` is minder handig.

Vanaf Java 8 en verder hebben we ook `String.join()` wat best netjes is voor het samenvoegen van verzamelingen strings met een scheidingsteken.

## Zie ook

- [De documentatie van de `String` klasse](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [De documentatie van de `StringBuilder` klasse](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [De documentatie van de `StringBuffer` klasse](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuffer.html)
- [Oracle's Java tutorials over Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
