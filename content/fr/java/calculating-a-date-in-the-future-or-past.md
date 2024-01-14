---
title:                "Java: Calcul d'une date dans le futur ou le passé."
simple_title:         "Calcul d'une date dans le futur ou le passé."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi
Il y a plusieurs raisons pour lesquelles quelqu'un pourrait avoir besoin de calculer une date dans le futur ou le passé en programmation. Peut-être pour planifier des événements futurs ou pour effectuer des tâches basées sur une date spécifique. Quelle que soit la raison, savoir comment le faire peut être très utile.

# Comment
Calculer une date dans le futur ou le passé en Java peut être facile à faire avec la bonne méthode. Tout d'abord, il est important de comprendre comment les dates sont représentées en Java. Elles sont enregistrées sous forme de nombre de millisecondes écoulées depuis le 1er janvier 1970 à minuit. On peut également utiliser la classe `Calendar` pour effectuer des calculs de dates.

Voici un exemple de code qui calcule une date dans le futur :

```Java
// Créer un objet Calendar et le définir à la date actuelle
Calendar cal = Calendar.getInstance();

// Ajouter 1 mois à la date actuelle
cal.add(Calendar.MONTH, 1);

// Convertir en format de date
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
String dateFutur = sdf.format(cal.getTime());

// Afficher la date dans le futur
System.out.println(dateFutur); // Output: 27/08/2021
```

Et voici un exemple de code pour calculer une date dans le passé :

```Java
// Créer un objet Calendar et le définir à la date actuelle
Calendar cal = Calendar.getInstance();

// Soustraire 1 mois à la date actuelle
cal.add(Calendar.MONTH, -1);

// Convertir en format de date
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
String datePasse = sdf.format(cal.getTime());

// Afficher la date dans le passé
System.out.println(datePasse); // Output: 27/06/2021
```

# Plongée en profondeur
Maintenant que nous avons vu comment calculer une date dans le futur ou le passé en Java, il est important de comprendre les différentes méthodes que la classe `Calendar` offre pour effectuer ces calculs. En utilisant les constantes prédéfinies comme `Calendar.MONTH` ou `Calendar.YEAR`, il est possible d'ajouter ou de soustraire une certaine quantité de temps à une date donnée.

Il existe également d'autres classes Java, comme `LocalDateTime` ou `ZonedDateTime`, qui offrent des fonctionnalités plus avancées pour travailler avec les dates et les heures. Il est recommandé de les explorer pour trouver la meilleure méthode pour vos besoins spécifiques.

# Voir aussi
- [Java Documentation on Dates and Times](https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html)
- [Tutorial de Java sur la manipulation des dates et heures](https://www.baeldung.com/java-date-time-api)
- [Guide de référence Java pour la classe Calendar](https://www.jmdoudoux.fr/java/dej/chap-date_manipulee.htm#date_manipulation_cpe)