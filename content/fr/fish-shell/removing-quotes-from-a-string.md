---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:38:43.233353-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Retirer les guillemets d'une chaîne de caractères consiste à enlever ces fameux guillemets simples (' ') ou doubles (" ") de vos données textuelles. Les programmeurs font souvent cela pour assainir l'entrée ou préparer les données pour un traitement ultérieur sans l'encombrement des guillemets.

## Comment faire :

Fish possède une magie intégrée pour ce type de tâche. Utilisez la fonction `string` sans vous casser la tête. Voici quelques sortilèges :

```fish
# Exemple avec des guillemets simples
set quoted "'Bonjour, Monde !'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Sortie : Bonjour, Monde !

# Même chose avec des guillemets doubles
set double_quoted "\"Bonjour, Univers !\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Sortie : Bonjour, Univers !
```

## Plongée Profonde

Dans l'âge de pierre de la ligne de commande, vous deviez lutter avec `sed` ou `awk` pour retirer les guillemets ; un véritable entrelacs de barres obliques inversées et de drapeaux cryptiques. La fonction `string` de Fish est d'une ère plus récente, rendant le code plus propre et plus intuitif.

Les alternatives dans d'autres shells pourraient encore dépendre de ces anciens outils ou utiliser leurs propres méthodes intégrées comme l'expansion de paramètres de bash ou les modificateurs de zsh.

La fonction `string` va au-delà du simple retrait de guillemets. C'est un couteau suisse pour les opérations sur les chaînes de caractères dans Fish. Avec `string`, vous pouvez découper, diviser, joindre, ou même matcher des chaînes avec des expressions régulières directement dans votre terminal.

## Voir également

Plongez plus profondément dans `string` avec l'aide de la documentation officielle :
- [Documentation sur les chaînes de caractères de Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Pour la nostalgie ou lors de l'écriture de scripts avec des shells plus traditionnels, consultez :
- [Guide Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Expansion de paramètres Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
