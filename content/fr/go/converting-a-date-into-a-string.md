---
title:                "Go: Conversion d'une date en chaîne de caractères"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères est une tâche fréquente en programmation et est utile pour afficher des dates dans un format spécifique ou les enregistrer dans une base de données. Connaître les différentes méthodes et formats disponibles peut rendre cette tâche plus simple et efficace.

## Comment faire

Voici un exemple de code en Go pour convertir une date en chaîne de caractères en utilisant la fonction `Format` de la bibliothèque `time`. 

```Go
import (
    "fmt"
    "time"
)

func main() {
    t := time.Now() // Récupère la date et l'heure actuelles
    dateStr := t.Format("02/01/2006") // Convertit la date en chaîne au format jj/mm/aaaa
    fmt.Println(dateStr) // Affiche 28/05/2021
}
```

Vous pouvez personnaliser le format de sortie en utilisant une combinaison de lettres comme `02` pour les jours, `01` pour les mois et `2006` pour les années. En utilisant ces lettres, vous pouvez créer des formats tels que `01/02/06`, `01-02-2006` ou `Janvier 2, 2006`.

Un autre moyen de convertir une date en chaîne de caractères en utilisant la bibliothèque `strconv` et la fonction `Itoa` pour convertir des entiers en chaînes.

```Go
import (
    "fmt"
    "strconv"
    "time"
)

func main() {
    t := time.Now()
    day := strconv.Itoa(t.Day()) // Convertit l'entier du jour en chaîne
    month := strconv.Itoa(int(t.Month())) // Convertit l'entier du mois en chaîne
    year := strconv.Itoa(t.Year()) // Convertit l'entier de l'année en chaîne

    // Crée une chaîne au format jj/mm/aaaa en utilisant les chaînes du jour, mois et année
    dateStr := day + "/" + month + "/" + year
    fmt.Println(dateStr) // Affiche 28/05/2021
}
```

## Plongée en profondeur

Lorsque vous utilisez la fonction `Format` de la bibliothèque `time`, vous pouvez également spécifier la localisation (langue et fuseau horaire) en utilisant la méthode `WithLocation` et la passer en paramètre.

```Go
t := time.Now()
fmt.Println(t.Format("January 2, 2006", t.WithLocation(time.UTC))) // Affiche May 28, 2021
fmt.Println(t.Format("January 2, 2006", t.WithLocation(time.FixedZone("CET", 1*60*60)))) // Affiche May 28, 2021
```

Il est également possible de convertir une chaîne en date en utilisant la fonction `Parse` de la bibliothèque `time` et en spécifiant le format de la chaîne.

```Go
t, err := time.Parse("01-02-2006", "28-05-2021") // Convertit la chaîne en date en utilisant le format jj-mm-aaaa
if err != nil {
    fmt.Println(err)
}
fmt.Println(t) // Affiche 2021-05-28 00:00:00 +0000 UTC
```

## Voir aussi

- [Documentation sur la bibliothèque `time`](https://pkg.go.dev/time)
- [Guide de format de dates en Go](https://www.strfti.me/)
- [Bibliothèque `strconv` en Go](https://pkg.go.dev/strconv)