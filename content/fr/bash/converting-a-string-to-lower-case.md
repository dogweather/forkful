---
title:    "Bash: Conversion d'une chaîne de caractères en minuscules"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Bash, vous savez peut-être déjà que les chaînes de caractères peuvent parfois être source de confusion. Les différentes majuscules et minuscules peuvent rendre votre code inefficace et difficile à lire. Heureusement, il existe un moyen simple de résoudre ce problème : la conversion d'une chaîne en minuscules. Dans cet article, nous allons expliquer pourquoi et comment vous devriez le faire.

## Comment faire

La conversion d'une chaîne en minuscules est assez simple en Bash. Tout d'abord, vous devez utiliser la commande `tr` suivie des options `[:upper:]` et `[:lower:]` pour spécifier les caractères à convertir. Ensuite, vous devez rediriger la chaîne à convertir vers la commande `tr` avec `echo`. Enfin, vous pouvez stocker le résultat dans une variable ou l'imprimer directement à l'écran. Voici un exemple de code :

```bash
str="PROGRAMMATION BASH"
echo "$str" | tr '[:upper:]' '[:lower:]'
```

Lorsque vous exécutez ce code, la chaîne sera convertie en minuscules et le résultat sera affiché à l'écran :

```
programmation bash
```

Vous pouvez également stocker le résultat dans une variable :

```bash
lowercase_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')
echo "$lowercase_str"
```

## Plongée en profondeur

Maintenant que vous savez comment convertir une chaîne en minuscules en Bash, vous pouvez vous demander quels sont les autres avantages de cette technique. Eh bien, en plus de rendre votre code plus lisible, la conversion en minuscules peut également être utile pour la comparaison de chaînes. Dans de nombreux cas, il est plus facile de comparer des chaînes lorsqu'elles sont toutes en minuscules, car les majuscules et les minuscules seront alors ignorées. Cela peut être particulièrement utile lors de l'écriture de scripts de recherche ou de tri de données.

De plus, la conversion en minuscules peut également éviter certains bogues ou erreurs dans votre code. En effet, en Bash, les variables ne sont pas sensibles à la casse, ce qui signifie que `$str` et `$STR` seront considérés comme la même variable. Cela peut entraîner des imprécisions ou des problèmes lors de l'utilisation de variables dans des expressions ou des conditions. En convertissant systématiquement vos chaînes en minuscules, vous pouvez éviter ce type de problèmes.

## Voir aussi

Pour plus d'informations sur la conversion de chaînes en minuscules en Bash, vous pouvez consulter les ressources suivantes :

- La documentation officielle de la commande `tr` : https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Un tutoriel vidéo sur la conversion de chaînes en minuscules en Bash : https://www.youtube.com/watch?v=VHNpFHUGfyU
- Un forum de discussion sur l'utilisation de `tr` pour convertir des chaînes : https://stackoverflow.com/questions/2264428/convert-string-to-lowercase-in-bash