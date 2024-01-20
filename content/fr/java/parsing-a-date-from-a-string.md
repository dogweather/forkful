---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?  
La conversion d'une date en chaîne de caractère, aussi appelée l'analyse d'une date, permet de transformer une date écrite sous forme textuelle en une représentation standard de date. Les développeurs l'utilisent pour traiter efficacement les dates et les manipuler de manière flexible dans leurs applications.

## Comment faire :
Voilà comment transformer une chaîne en date en utilisant la classe `SimpleDateFormat` de Java.

```Java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
  public static void main(String[] args) {
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
    try {
      Date date = formatter.parse("31/12/2021");
      System.out.println(date);
    } catch (ParseException e) {
      e.getStackTrace();
    }
  }
}
```

Résultat de l'extrait de code précédent, 

```Java
Fri Dec 31 00:00:00 CET 2021
```

## Plongée en profondeur

L'analyse de date a été présente dès les premières versions de Java. Cependant, la gestion des dates a été grandement améliorée avec l'introduction de Java 8. 

Parmi les alternatives, nous avons la classe `DateTimeFormatter` de Java 8 qui offre une meilleure gestion des erreurs et une plus grande flexibilité.

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
  public static void main(String[] args) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    LocalDate date = LocalDate.parse("31/12/2021", formatter);
    System.out.println(date);
  }
}
```
Vous remarquerez que grâce à cette classe, nous avons évité d'utiliser un bloc `try-catch` pour gérer les exceptions de conversion.

Le choix de l'une ou l'autre méthode dépend largement des besoins spécifiques de votre projet. Par exemple, `SimpleDateFormat` n'est pas thread-safe, ce qui peut être un problème dans les applications multithread, alors que `DateTimeFormatter` l'est.

## Voir aussi

Pour plus de détails, consultez la documentation officielle de Java sur l'analyse des dates :

1. [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
2. [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)