---
title:    "Elm: Utiliser les expressions régulières"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi
Les expressions régulières sont un outil puissant et polyvalent pour manipuler et rechercher du texte dans vos programmes Elm. Ils vous permettent de trouver et de manipuler des motifs spécifiques dans une chaîne de caractères, ce qui est particulièrement utile lors de la validation de données ou de la recherche d'informations dans des fichiers.

## Comment faire
Pour utiliser les expressions régulières en Elm, vous devez d'abord importer le module Regex. Ensuite, vous pouvez créer un modèle en utilisant la fonction `regex` suivie du motif que vous souhaitez chercher. Par exemple, si vous voulez trouver tous les nombres dans une chaîne de caractères, vous pouvez utiliser le modèle `regex "\\d+"`.

Une fois que vous avez créé votre modèle, vous pouvez l'utiliser pour effectuer différentes opérations telles que la validation de données ou la recherche de motifs dans une chaîne de caractères. Voici un exemple de code Elm montrant comment utiliser une expression régulière pour valider un numéro de téléphone :

```Elm
import Regex

-- Définition du modèle pour un numéro de téléphone au format (XXX) XXX-XXXX
let phoneRegex = Regex.regex "\\(\\d{3}\\) \\d{3}-\\d{4}"

-- Fonction pour valider un numéro de téléphone en utilisant le modèle
let validatePhone number =
  Regex.contains phoneRegex number

-- Tests
validatePhone "(123) 456-7890"  -- renvoie True
validatePhone "123-456-7890"    -- renvoie False
```

Dans l'exemple ci-dessus, nous utilisons le modèle `phoneRegex` pour vérifier si un numéro de téléphone est au bon format. Vous pouvez également utiliser les expressions régulières pour extraire des données d'une chaîne de caractères en associant le motif à une partie de la chaîne à l'aide de la fonction `Regex.find` ou `Regex.replace`.

## Plongée en profondeur
Les expressions régulières sont un outil flexible et puissant, mais elles peuvent être délicates à comprendre au début. Il existe de nombreux motifs et opérateurs différents que vous pouvez utiliser pour correspondre à des motifs spécifiques. Certains patrons courants incluent `\d` pour correspondre à n'importe quel chiffre, `\w` pour correspondre à n'importe quel caractère alphanumérique et `[a-z]` pour correspondre à n'importe quel caractère de la plage donnée.

Pour en savoir plus sur les expressions régulières en Elm, vous pouvez consulter les ressources suivantes :

- Documentation officielle Elm : [Module Regex]()https://package.elm-lang.org/packages/elm/regex/latest/)
- Tutoriel vidéo sur les expressions régulières en Elm : [Expressions régulières en Elm avec Kris Jenkins](https://www.youtube.com/watch?v=UNRuQ3zNaeE)
- Tutoriel écrit sur les expressions régulières en Elm : [Utiliser des expressions régulières en Elm](https://www.andrewgharrington.com/why-you-should-use-regular-expressions-in-elm/)

## Voir aussi
- [Expressions régulières en Elm sur la documentation officielle](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Vidéo YouTube sur les expressions régulières en Elm](https://www.youtube.com/watch?v=UNRuQ3zNaeE)
- [Tutoriel écrit sur les expressions régulières en Elm](https://www.andrewgharrington.com/why-you-should-use-regular-expressions-in-elm/)