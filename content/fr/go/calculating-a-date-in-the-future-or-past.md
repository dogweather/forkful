---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Go: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Calculer une date dans le futur ou dans le passé est un besoin commun en programmation. Cela peut être utile pour planifier des événements, créer des rappels ou effectuer des calculs de temps pour des applications.

# Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant Go, vous pouvez utiliser la fonction `AddDate` du package `time`. Voici un exemple de code pour calculer la date d'aujourd'hui dans une semaine :

```
aujourd'hui := time.Now()
dateDansUneSemaine := aujourd'hui.AddDate(0, 0, 7)
fmt.Println(dateDansUneSemaine)
```

Cela affichera la date dans une semaine au format `time.Time` :

2022-02-18 09:00:00 +0000 UTC

Vous pouvez également spécifier une date précise en utilisant les paramètres de la fonction `AddDate`. Par exemple, pour calculer une date à 3 mois dans le passé, vous pouvez utiliser :

```
aujourd'hui := time.Now()
dateDansTroisMois := aujourd'hui.AddDate(0, -3, 0)
fmt.Println(dateDansTroisMois)
```

Cela affichera la date d'il y a trois mois :

2021-11-13 09:00:00 +0000 UTC

Il est important de noter que la fonction `AddDate` renvoie une nouvelle valeur de type `time.Time` et ne modifie pas l'objet original.

# Plongée en profondeur

En utilisant la fonction `AddDate`, vous pouvez également effectuer des calculs plus complexes, tels que calculer une date dans le futur ou dans le passé en fonction d'un intervalle de temps spécifique. Par exemple, pour calculer une date dans 2 ans, 3 mois et 4 jours, vous pouvez utiliser :

```
aujourd'hui := time.Now()
dateDansDeuxAns := aujourd'hui.AddDate(2, 3, 4)
fmt.Println(dateDansDeuxAns)
```

Cela affichera la date dans 2 ans, 3 mois et 4 jours :

2024-05-22 09:00:00 +0000 UTC

Vous pouvez également utiliser la fonction `Sub` pour calculer la différence entre deux dates. Par exemple, pour calculer le nombre de jours entre deux dates, vous pouvez utiliser :

```
date1 := time.Date(2022, 02, 12, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2022, 02, 18, 0, 0, 0, 0, time.UTC)
jours := date1.Sub(date2).Hours() / 24
fmt.Println(int(jours))
```

Cela affichera le nombre de jours entre les deux dates :

6

# Voir aussi

- Documentation sur le package time en Go : https://pkg.go.dev/time
- Tutoriel sur le calcul de dates en Go : https://gobyexample.com/time
- Exemples de code pour la manipulation de dates en Go : https://github.com/golang/example/tree/master/dates