---
title:                "Go: Calcul d'une date dans le futur ou le passé"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent utile dans le développement de logiciels de pouvoir calculer une date dans le futur ou dans le passé, que ce soit pour planifier des tâches ou pour afficher des informations temporelles. Dans cet article, nous allons explorer comment réaliser ce calcul en langage Go.

## Comment Faire

Les dates en Go sont représentées par des objets de type `time.Time`. Pour calculer une date dans le futur ou dans le passé, nous devons utiliser les méthodes `Add` ou `Sub` de cet objet.

Voici un exemple de code pour calculer une date dans le futur en ajoutant un nombre de jours spécifié à la date actuelle :

```Go
// Déclarer une variable de type time.Time avec la valeur de la date actuelle
dateActuelle := time.Now()

// Ajouter 7 jours à la date actuelle
dateFuture := dateActuelle.AddDate(0, 0, 7)

// Afficher la date dans le format "January 2, 2006"
fmt.Println("Date future :", dateFuture.Format("January 2, 2006"))

// Sortie : Date future : January 23, 2021
```

Et voici un exemple pour calculer une date dans le passé en soustrayant un nombre de mois spécifié à la date actuelle :

```Go
// Déclarer une variable de type time.Time avec la valeur de la date actuelle
dateActuelle := time.Now()

// Soustraire 6 mois à la date actuelle
datePasse := dateActuelle.AddDate(0, -6, 0)

// Afficher la date dans le format "January 2, 2006"
fmt.Println("Date passée :", datePasse.Format("January 2, 2006"))

// Sortie : Date passée : July 23, 2020
```

## Plongée Profonde

En utilisant la méthode `AddDate` ou `SubDate`, nous pouvons spécifier un nombre de jours, de mois ou d'années pour le calcul de la date. De plus, ces méthodes prennent également en compte les années bissextiles et ajustent automatiquement la date en conséquence.

Il est également possible de combiner ces méthodes avec d'autres fonctions de manipulation du temps, telles que la conversion en fuseau horaire spécifique ou la comparaison de dates.

## Voir Aussi

- [Documentation officielle de la librairie time en Go](https://golang.org/pkg/time/)
- [Article sur la manipulation des dates en Go](https://medium.com/@oanaoloariu/manipulating-dates-and-time-in-go-914f4fd4a4e6)
- [Exercice pratique sur le calcul de date en Go](https://gophercises.com/exercises/time)

Merci d'avoir lu cet article sur le calcul de date en Go ! Nous espérons que cela vous a été utile dans vos projets de développement. N'hésitez pas à explorer davantage les possibilités offertes par la librairie `time` pour manipuler et gérer le temps dans vos applications. À la prochaine !