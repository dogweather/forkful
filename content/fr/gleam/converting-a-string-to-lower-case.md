---
title:                "Gleam: Convertir une chaîne de caractères en minuscules"
simple_title:         "Convertir une chaîne de caractères en minuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est parfois nécessaire de convertir une chaîne de caractères en minuscules. Que ce soit pour une vérification de mot de passe, une comparaison de chaînes ou simplement pour des raisons esthétiques, cette fonctionnalité est largement utilisée dans de nombreux langages de programmation. Dans cet article, nous allons voir comment convertir une chaîne de caractères en minuscules avec Gleam.

## Comment faire

Pour convertir une chaîne de caractères en minuscules avec Gleam, nous allons utiliser la fonction `String.to_lower` qui prend en paramètre une chaîne de caractères et renvoie une nouvelle chaîne avec tous les caractères en minuscules.

```Gleam
let str = "Bonjour, MONDE!"
let result = String.to_lower(str)
io.println(result)
```

Lorsque nous exécutons ce code, la sortie sera `bonjour, monde!`. Comme vous pouvez le voir, tous les caractères sont maintenant en minuscules.

## Plongée en profondeur

Il est important de noter que la fonction `String.to_lower` utilise l'Unicode pour effectuer la conversion. Cela signifie que les caractères spéciaux ou accentués seront également convertis en minuscules selon la règle de normalisation Unicode. Par exemple, "Éléphant" sera converti en "éléphant" et "Über" en "über".

Si vous souhaitez uniquement convertir les lettres de l'alphabet anglais en minuscules, vous pouvez utiliser la fonction `String.to_lower_ascii` qui ne tient compte que des 26 lettres de l'alphabet anglais dans la conversion.

## Voir aussi

- Documentation officielle de Gleam : https://gleam.run/documentation/
- Tutoriels pour débutants : https://gleam.run/documentation/tutorials/
- Discussion sur Stack Overflow : https://stackoverflow.com/questions/tagged/gleam