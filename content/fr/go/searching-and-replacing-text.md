---
title:                "Go: Recherche et remplacement de texte"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

L'une des fonctionnalités les plus importantes dans n'importe quel langage de programmation est la capacité de rechercher et de remplacer du texte. Que ce soit pour corriger des erreurs dans votre code ou pour mettre à jour des données, la recherche et le remplacement de texte peuvent vous faire gagner un temps précieux. Dans cet article, nous allons explorer comment le faire en utilisant le langage de programmation Go.

# Comment faire

Pour effectuer une recherche et un remplacement de texte en Go, nous allons utiliser la fonction `strings.Replace()`. Cette fonction prend trois arguments : la chaîne de caractères dans laquelle nous voulons effectuer la recherche, la chaîne de caractères à chercher, et la chaîne de caractères de remplacement. Voici un exemple de code pour remplacer toutes les occurrences de "go" par "golang" dans une chaîne de caractères :

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	texte := "J'aime coder en go. C'est mon langage de programmation préféré."
	nouveauTexte := strings.Replace(texte, "go", "golang", -1)
	fmt.Println(nouveauTexte)
}
```

La ligne `strings.Replace(texte, "go", "golang", -1)` remplace toutes les occurrences de "go" par "golang" dans la chaîne de caractères `texte` et stocke le nouveau texte dans la variable `nouveauTexte`. Notez que le troisième argument `-1` indique que toutes les occurrences doivent être remplacées.

L'exemple ci-dessus produit la sortie suivante :

```
J'aime coder en golang. C'est mon langage de programmation préféré.
```

# Plongée en profondeur

La fonction `strings.Replace()` ne fonctionne que pour les chaînes de caractères simples. Mais si vous voulez effectuer une recherche et un remplacement dans des fichiers plus complexes, tels que des fichiers texte, vous devrez utiliser des expressions régulières.

Pour cela, Go dispose de la bibliothèque standard `regexp`. Voici un exemple de code pour remplacer toutes les occurrences de "Go" ou "golang" dans un fichier texte par "GoLang" :

```
package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
)

func main() {
	// Lire le fichier texte
	fichier, _ := ioutil.ReadFile("monFichier.txt")
	// Convertir le binaire en chaîne de caractères
	texte := string(fichier)
	
	// Expression régulière pour trouver "go" ou "golang"
	reg := regexp.MustCompile(`go|golang`)

	// Remplacer toutes les occurrences par "GoLang"
	nouveauTexte := reg.ReplaceAllString(texte, "GoLang")

	fmt.Println(nouveauTexte)
}
```

L'exemple ci-dessus utilise la fonction `regexp.MustCompile()` pour compiler une expression régulière avec les options spécifiées. Puis, la fonction `reg.ReplaceAllString()` est utilisée pour remplacer toutes les occurrences de "go" ou "golang" par "GoLang".

# Voir aussi

- [Documentation officielle de Go sur la fonction strings.Replace()](https://golang.org/pkg/strings/#Replace)
- [Documentation officielle de Go sur la bibliothèque regexp](https://golang.org/pkg/regexp/)
- [Tutoriel vidéo sur la recherche et le remplacement de texte en utilisant Go](https://www.youtube.com/watch?v=XaMr--wAuSI)