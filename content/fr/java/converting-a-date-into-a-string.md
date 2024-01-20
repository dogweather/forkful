---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La conversion d'une date en chaîne de caractères est une tâche courante en Java. Elle permet aux programmeurs de manipuler, d'afficher ou de stocker des dates dans un format plus convivial.

## Comment faire :

Voici un exemple de comment faire cela en utilisant SimpleDateFormat :

```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
  public static void main(String[] args) {
    // Créez un objet Date
    Date date = new Date();

    // Créez un objet SimpleDateFormat
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");

    // Convertissez la date en String
    String strDate= formatter.format(date);

    // Imprimez la date en String
    System.out.println("Date formatée en String: " + strDate);
  }
}
```

Lorsque vous exécutez ce programme, le résultat serait une date formatée en String, par exemple:

```
Date formatée en String: 24/12/2022
```

## Immersion :

### Contexte historique 
La classe SimpleDateFormat est en usage depuis JDK 1.1. Pour les versions Java 8 et supérieures, la classe DateTimeFormatter est recommandée.

### Alternatives
Une alternative à SimpleDateFormat est la classe DateTimeFormatter (disponible depuis Java 8). Le code serait :

```java
import java.time.format.DateTimeFormatter;
import java.time.LocalDateTime;

public class Main {
  public static void main(String[] args) {
    // Créez un objet LocalDateTime
    LocalDateTime now = LocalDateTime.now();

    // Créez un objet DateTimeFormatter
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    // Convertissez la date en String
    String formatDateTime = now.format(formatter);

    // Imprimez la date en String
    System.out.println("Date formatée en String: " + formatDateTime);
  }
}
```

### Détails de mise en œuvre
Lors de la conversion d'une date en chaîne, il est important de gérer les exceptions ParseException et DateTimeParseException qui peuvent être générées lors de l'interprétation de la chaîne de date.

## Voir Aussi :

Pour plus d'informations sur les différentes classes et méthodes liées à la conversion de dates en chaînes de caractères en Java, vous pouvez consulter : 

- [Java DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Java LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)