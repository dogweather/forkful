---
title:                "Passage à la casse en minuscules d'une chaîne de caractères"
html_title:           "Go: Passage à la casse en minuscules d'une chaîne de caractères"
simple_title:         "Passage à la casse en minuscules d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?
Convertir une chaîne de caractères en minuscules signifie simplement modifier toutes les lettres en minuscules. Les programmeurs le font souvent pour faciliter la comparaison de chaînes de caractères ou pour s'assurer que toutes les données sont dans le même format.

## Comment le faire:
Go rend la conversion de chaîne en minuscules très simple avec la fonction intégrée `strings.ToLower()`. Voici un exemple de code pour convertir une chaîne en minuscules:

```Go
maChaine := "HELLO"
minuscules := strings.ToLower(maChaine)
fmt.Println(minuscules) // output: hello
```

## Plongée en profondeur:
La conversion de chaîne en minuscules est une opération courante et simple, mais elle peut être utile de savoir que cela a été une fonctionnalité ajoutée tardivement à Go. Auparavant, les programmeurs devaient utiliser des fonctions telles que `strings.ToLowerSpecial()` pour effectuer la même tâche. De plus, il existe des alternatives telles que l'utilisation de la méthode `Lower()` sur un objet de type `strings.Builder` ou l'utilisation de la bibliothèque `unicode` pour gérer les caractères spéciaux lors de la conversion.

## Voir aussi:
Pour plus d'informations sur la manipulation de chaînes en Go, vous pouvez consulter la documentation officielle de Go sur les chaînes: https://golang.org/pkg/strings/
Vous pouvez également découvrir d'autres fonctions pour manipuler les chaînes dans la bibliothèque `strings` et `unicode`.