---
title:                "Concaténer des chaînes de caractères"
html_title:           "Gleam: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ? 
La concaténation de chaînes est le processus de fusionner plusieurs chaînes de caractères en une seule. Les programmeurs utilisent cette technique pour créer des phrases complètes ou pour créer des messages d'erreur personnalisés.

## Comment faire :
Voici un exemple de code Gleam montrant comment concaténer des chaînes de caractères :

```
Gleam 
    fn concatener() {
    let nom = "Marie"
    let prenom = "Dupont"
    let phrase = nom + " " + prenom
    IO.format("{:?}", phrase)
}
```

Lorsque vous exécutez cette fonction, vous obtenez une phrase complète en sortie : "Marie Dupont". La concaténation des chaînes est particulièrement utile lorsque vous devez combiner des variables avec du texte pour créer des chaînes dynamiques.

## Plongée en profondeur :
La concaténation de chaînes est une technique couramment utilisée dans de nombreux langages de programmation, y compris Gleam. Cependant, d'autres approches peuvent être utilisées, telles que la formatage de chaînes, qui utilise des marqueurs pour insérer des variables dans une chaîne. En interne, Gleam utilise le type 'String' pour gérer les chaînes de caractères et offre plusieurs méthodes pour concaténer des chaînes. Par exemple, vous pouvez utiliser l'opérateur '+' ou la fonction 'String.concat' pour réaliser une concaténation.

## À voir aussi :
Pour en savoir plus sur la concaténation de chaînes en Gleam, vous pouvez consulter la documentation officielle : https://gleam.run/documentation/guides/strings.html. Vous pouvez également jeter un œil aux tutoriels et exemples de code disponibles sur le site Gleam : https://gleam.run/documentation/. N'hésitez pas à explorer et à expérimenter par vous-même pour devenir un pro de la concaténation de chaînes en Gleam !