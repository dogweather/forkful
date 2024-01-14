---
title:    "Go: Supprimer les caractères correspondant à un motif"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Go, la suppression de caractères correspondant à un motif peut être utile pour nettoyer une chaîne de caractères ou pour effectuer une recherche et un remplacement dans un fichier texte. Cela peut également être utilisé pour extraire des données spécifiques d'un texte.

## Comment faire

Voici un exemple de code en Go pour supprimer tous les chiffres d'une chaîne de caractères :

```
texte := "J'aime les 123 pommes"
nouveauTexte := ""
for _, caractere := range texte {
    if caractere < '0' || caractere > '9' {
        nouveauTexte += string(caractere)
    }
}
fmt.Println(nouveauTexte)
```
Output :

```
J'aime les pommes
```

Vous pouvez également utiliser des expressions régulières pour supprimer des caractères spécifiques en utilisant le package `regexp` :

```
texte := "Mon adresse email est go@gmail.com"
regExp := regexp.MustCompile("[^a-zA-Z@.]")
nouveauTexte := regExp.ReplaceAllString(texte, "")
fmt.Println(nouveauTexte)
```

Output :

```
Monadresseemailestgo@gmailcom
```

## Plongée en profondeur

La suppression de caractères correspondant à un motif peut être réalisée de différentes manières en Go. En utilisant le package `strings`, vous pouvez aussi utiliser la méthode `Replace` pour remplacer un motif par une chaîne vide. Vous pouvez également utiliser les packages `bufio` et `os` pour effectuer une suppression dans un fichier texte.

## Voir aussi

- [Documentation officielle Go](https://golang.org/doc/)
- [Documentation sur les expressions régulières en Go](https://golang.org/pkg/regexp/)
- [Blog de la communauté Go](https://blog.golang.org/)