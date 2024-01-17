---
title:                "Utiliser les expressions régulières"
html_title:           "Go: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Qu'est-ce que les expressions régulières et pourquoi les programmeurs les utilisent-ils?

Les expressions régulières sont un outil puissant pour manipuler et rechercher des motifs dans des chaînes de caractères. De nombreux programmeurs les utilisent pour effectuer des tâches telles que la validation de données utilisateur, le filtrage de chaînes de caractères et la recherche de motifs spécifiques.

# Comment faire:

```
Go: package main import "fmt" import "regexp"

func main() {
    // Créer une expression régulière pour rechercher le motif "go"
    regex := regexp.MustCompile("go")

    // Rechercher le motif dans une chaîne de caractères
    result := regex.FindString("Je suis un programmeur Go")

    // Afficher le résultat
    fmt.Println(result) // Output: go
}
```

# Plongée en profondeur:

Les expressions régulières ont été inventées dans les années 1950 et ont depuis été utilisées dans de nombreux langages de programmation. Cependant, avec l'avènement du langage Go, les programmeurs ont maintenant accès à une manière plus simple et plus efficace d'utiliser les expressions régulières. Les alternatives à l'utilisation des expressions régulières incluent la manipulation de chaînes de caractères en utilisant des méthodes de manipulation de chaînes de base, mais cela peut être fastidieux et moins robuste.

# Voir aussi:

- [Documentation officielle de Go sur les expressions régulières](https://golang.org/pkg/regexp/)
- [Article sur les expressions régulières en Go](https://www.wiut.uz/uploads/CSE/regular-expressions-in-go-pdf.pdf) par Jaehoon Lim