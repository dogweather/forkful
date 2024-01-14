---
title:                "Go: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Souvent, lors de l'écriture de code, il est nécessaire de modifier rapidement certains passages de texte. Par exemple, remplacer un nom de variable dans l'ensemble du code ou corriger une faute de frappe.

## Comment faire

Avec Go, il existe une fonction intégrée pour effectuer des recherches et des remplacements de texte. Elle s'appelle "Replace" et elle est très simple à utiliser.

Voici un exemple de code pour remplacer le nom d'une variable "age" par "ageInYears":

```Go
str := "La personne a 25 ans."
nouvelleStr := strings.Replace(str, "age", "ageInYears", -1)
fmt.Println(nouvelleStr)
```

Résultat:

    La personne a 25 ans.

Vous pouvez également spécifier le nombre de remplacements maximaux à effectuer. Par exemple, pour remplacer uniquement le premier "age" par "ageInYears":

```Go
str := "La personne a 25 ans."
nouvelleStr := strings.Replace(str, "age", "ageInYears", 1)
fmt.Println(nouvelleStr)
```

Résultat:

    La personne a 25 ans.

Il est également possible de passer en revue un fichier entier et de remplacer tous les mots ou une expression spécifique. Voici un exemple de code pour remplacer toutes les occurrences de "rum" par "whisky" dans un fichier:

```Go
fichier, err := ioutil.ReadFile("boissons.txt")
if err != nil {
    log.Fatal(err)
}
nouveauFichier := strings.Replace(string(fichier), "rum", "whisky", -1)
err = ioutil.WriteFile("boissons_modifie.txt", []byte(nouveauFichier), 0644)
if err != nil {
    log.Fatal(err)
}
```

## Plongée en profondeur

La fonction "Replace" utilise une chaîne de caractères pour spécifier le texte à rechercher et un autre pour le remplacer. Mais il existe également une variante de cette fonction qui prend une expression régulière pour une recherche plus précise. Elle s'appelle "ReplaceAllString" et voici un exemple de code pour l'utiliser:

```Go
str := "Ceci est une phrase."
nouvelleStr := regexp.MustCompile("une (.+)\.")
nouvellePhrase := "une incroyable $1!"
resultat := nouvelleStr.ReplaceAllString(str, nouvellePhrase)
fmt.Println(resultat)
```

Résultat:

    Ceci est une incroyable phrase!

## Voir aussi

- Documentation officielle de la fonction strings.Replace: https://golang.org/pkg/strings/#Replace
- Tutoriel sur les expressions régulières en Go: https://blog.golang.org/regular-expressions
- Exemples de codes dans cet article: https://play.golang.org/p/6M8izs065pY