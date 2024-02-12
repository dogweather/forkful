---
title:                "Obtenir la date actuelle"
aliases: - /fr/java/getting-the-current-date.md
date:                  2024-02-03T19:09:45.717129-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle en Java est une opération fondamentale qui permet aux programmeurs de manipuler des objets de date pour des opérations telles que la journalisation, les calculs de dates et les conditions basées sur le temps. C'est essentiel dans les applications où le suivi, la planification et l'analyse des données temporelles sont cruciaux.

## Comment faire :
Java offre plusieurs moyens d'obtenir la date actuelle, en utilisant à la fois l'ancienne classe `java.util.Date` et le nouveau package `java.time` (introduit dans Java 8) qui est plus polyvalent et intuitif.

### Utilisation de `java.time.LocalDate`
```java
import java.time.LocalDate;

public class ExempleDateActuelle {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Exemple de sortie : 2023-04-01
    }
}
```
### Utilisation de `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class ExempleDateActuelle {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Exemple de sortie : 2023-04-01T12:34:56.789
    }
}
```
### Utilisation de `java.util.Date` (Ancien)
```java
import java.util.Date;

public class ExempleDateActuelle {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Exemple de sortie : Sam Apr 01 12:34:56 BST 2023
    }
}
```
### Utilisation d'une bibliothèque tierce : Joda-Time
Avant Java 8, Joda-Time était la norme de facto pour la date et l'heure en Java. Si vous travaillez sur des systèmes hérités ou si vous avez une préférence pour Joda-Time, voici comment vous pouvez l'utiliser pour obtenir la date actuelle :
```java
import org.joda.time.LocalDate;

public class ExempleDateActuelle {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Exemple de sortie : 2023-04-01
    }
}
```
**Note :** Bien que `java.util.Date` et Joda-Time soient toujours utilisés, le package `java.time` est recommandé pour les nouveaux projets en raison de son immuabilité et de son API complète pour la gestion des dates et des heures.
