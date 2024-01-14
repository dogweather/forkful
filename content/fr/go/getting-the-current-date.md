---
title:                "Go: Obtention de la date actuelle"
simple_title:         "Obtention de la date actuelle"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur en Go, vous avez peut-être besoin de récupérer la date actuelle dans votre programme. Cela peut être utile pour afficher l'heure exacte d'un événement, pour des tâches de planification ou tout simplement pour suivre le temps. Dans cet article, nous allons expliquer comment obtenir la date actuelle en utilisant Go.

## Comment faire

Pour obtenir la date actuelle en Go, nous allons utiliser la fonction `Now()` de la bibliothèque `time`. Voici un exemple de code qui montre comment obtenir la date actuelle en utilisant `Now()` :

```
Go time.Now()
```

Voici un exemple de sortie : `2021-09-29 16:30:00 +0000 UTC`.

En utilisant `Now()`, vous obtiendrez la date et l'heure au format UTC. Si vous souhaitez obtenir la date et l'heure dans un fuseau horaire spécifique, vous pouvez utiliser la méthode `In()` après `Now()`. Voici un exemple :

```
Go time.Now().In(time.UTC)
```

Cela vous donnera la date et l'heure dans le fuseau horaire UTC.

Il est également possible d'obtenir la date et l'heure dans d'autres fuseaux horaires en utilisant la méthode `LoadLocation()`. Voici un exemple :

```
Go time.Now().LoadLocation("Europe/Paris")
```

Ce code donnera la date et l'heure dans le fuseau horaire de Paris. Vous pouvez remplacer "Europe/Paris" par n'importe quel fuseau horaire disponible.

## Plongée en profondeur

En utilisant la bibliothèque `time` de Go, vous pouvez également formater la date et l'heure selon vos besoins. Par exemple, si vous voulez afficher uniquement l'année, le mois et le jour, vous pouvez utiliser la méthode `Format()`. Voici un exemple :

```
Go time.Now().Format("2006-01-02")
```

Cela donnera le résultat suivant : `2021-09-29`.

Vous pouvez également utiliser `Format()` pour afficher l'heure au format 12 heures au lieu de 24 heures. Voici un exemple :

```
Go time.Now().Format("03:04:05 PM")
```

Ce code donnera l'heure au format 12 heures avec les minutes et les secondes. Vous pouvez utiliser différents formats pour afficher la date et l'heure de différentes manières.

## Voir aussi

- [Golang Time Package Documentation](https://golang.org/pkg/time/)
- [Golang Tutorial: Getting the Current Date and Time](https://golangbot.com/get-current-date-time/)
- [How to Format Date and Time in Go](https://www.calhoun.io/how-to-format-date-and-time-in-go/)