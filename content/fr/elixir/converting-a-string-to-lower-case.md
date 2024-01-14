---
title:                "Elixir: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Les chaînes de caractères sont un élément important dans la programmation, et il peut être utile de pouvoir les manipuler et les formater selon nos besoins. La conversion d'une chaîne de caractères en lettres minuscules en est un exemple. Cela peut sembler simple, mais il y a des raisons importantes pour lesquelles cela peut être utile dans vos projets en Elixir.

## Comment faire

Il existe plusieurs façons de convertir une chaîne de caractères en minuscules en Elixir. Voici deux exemples utilisant différentes méthodes.

```Elixir
string = "HELLO WORLD!"
lowercased = String.downcase(string)
```
Sortie :
```
"hello world!"
```

Nous pouvons également utiliser la fonction `string.downcase` sur une liste de caractères pour obtenir le même résultat.

```Elixir
string = "HELLO WORLD!"
lowercased = String.downcase([65, 66, 67, 68, 69, 70])
``` 
Sortie : 
``` 
["a", "b", "c", "d", "e", "f"]
```

Il est important de noter que la fonction `String.downcase` n'affecte pas la chaîne originale, mais renvoie plutôt une nouvelle chaîne en lettres minuscules. 

## Plongée en profondeur

Pour les utilisateurs plus avancés, il peut être intéressant de savoir que la conversion en minuscules en Elixir utilise l'algorithme Unicode pour prendre en compte les caractères spéciaux et les accents. Cela permet une conversion précise et cohérente dans toutes les langues. De plus, en utilisant la méthode `String.downcase`, les caractères hors plage ASCII seront également convertis en minuscules.

## Voir aussi

- Documentation officielle d'Elixir sur les chaînes de caractères: https://hexdocs.pm/elixir/String.html
- Article sur les façons de manipuler les chaînes en Elixir : https://medium.com/@prathammalik/elixir-strings-hands-on-a72996a17ec1