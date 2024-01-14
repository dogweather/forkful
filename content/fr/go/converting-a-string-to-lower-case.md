---
title:    "Go: Convertir une chaîne en lettres minuscules"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules peut être utile dans divers cas de programmation. Par exemple, cela peut faciliter la comparaison de chaînes de caractères ou garantir une uniformité dans la saisie de données.

## Comment faire

La conversion d'une chaîne de caractères en minuscules en Go est assez simple. Tout ce que vous avez à faire est d'utiliser la fonction "strings.ToLower()" en lui passant en paramètre la chaîne que vous souhaitez convertir. Voici un exemple de code en Go :

```Go
s := "BONJOUR"
fmt.Println(strings.ToLower(s))
```

Lorsque vous exécuterez ce code, vous obtiendrez la sortie suivante :

```
bonjour
```

## Exploration approfondie

La fonction "strings.ToLower()" en Go utilise le package "strings" qui propose une multitude de fonctions de manipulation de chaînes de caractères. En creusant un peu plus, vous découvrirez que cette fonction utilise également le package "unicode" pour gérer correctement les caractères Unicode lors de la conversion en minuscules.

N'hésitez pas à explorer ces packages et à découvrir toutes les fonctionnalités intéressantes qu'ils offrent pour la manipulation de chaînes de caractères en Go.

## Voir aussi

- [Documentation officielle sur la fonction strings.ToLower()](https://golang.org/pkg/strings/#ToLower)
- [Package "strings" en Go](https://golang.org/pkg/strings/)
- [Package "unicode" en Go](https://golang.org/pkg/unicode/)