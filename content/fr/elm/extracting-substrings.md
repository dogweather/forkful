---
title:    "Elm: Extraction de sous-chaînes de caractères"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

Extractions de sous-chaînes, ou substring en anglais, est une fonctionnalité courante dans de nombreux langages de programmation. Il s'agit d'une méthode utile pour récupérer une partie spécifique d'une chaîne de caractères, que ce soit pour la manipuler ou pour l'afficher à l'utilisateur. Dans cet article, nous allons explorer comment implémenter cette fonctionnalité en utilisant Elm.

## Comment faire

```elm
-- Déclaration d'une chaîne de caractères
let texte = "Bonjour tout le monde"

-- Récupérer la sous-chaîne de caractères à partir de l'index 8 (inclu)
-- jusqu'à l'index 12 (exclu)
let sousChaine = String.slice 8 12 texte

-- Afficher la sous-chaîne
Debug.log "Sous-chaîne:" sousChaine
```

Lorsque nous exécutons ce code, nous obtenons "tout" comme sortie, car la fonction `String.slice` récupère une partie de la chaîne en utilisant les indexes fournis. En spécifiant l'index de départ et l'index de fin (exclu), nous pouvons extraire une portion spécifique de notre chaîne de caractères.

Une autre méthode de récupération de sous-chaînes est `String.take`, qui permet de spécifier uniquement l'index de fin et renvoie la sous-chaîne à partir du début de la chaîne jusqu'à l'index spécifié (exclu). De même, `String.drop` renvoie la sous-chaîne à partir de l'index spécifié (inclu) jusqu'à la fin de la chaîne.

## Plongée en profondeur

Maintenant que nous avons vu comment extraire des sous-chaînes en utilisant les fonctions prédéfinies de Elm, allons un peu plus en détail. Ces fonctions nous permettent également de spécifier un nœud pour commencer l'extraction en utilisant le paramètre optionnel `offset`, qui par défaut est fixé à 0. Par exemple, en utilisant `String.slice 2 5 "Exemple"`, nous obtenons "emp" comme résultat. Cependant, si nous spécifions un `offset` de 1, le résultat sera "xem".

Il est également possible de fournir un second paramètre optionnel `step` qui spécifie l'incrément, c'est-à-dire l'écart entre les indexes. Lors de l'utilisation de cette option, le `offset` devient obligatoire pour indiquer le nœud de départ de l'extraction.

Il est important de noter que ces fonctions de manipulation de sous-chaînes ne modifient pas la chaîne originale, mais renvoient plutôt une nouvelle chaîne en fonction des paramètres fournis.

## Voir aussi

Pour plus d'informations sur les fonctions de manipulation de chaînes de caractères en Elm, consultez la documentation officielle : https://guide.elm-lang.org/strings/. Vous pouvez également explorer d'autres astuces et exemples pratiques sur la communauté Elm : https://elm-lang.org/community. N'hésitez pas à expérimenter et à partager vos découvertes avec la communauté. Bon codage!