---
title:                "Gleam: Imprimer les sorties de débogage"
simple_title:         "Imprimer les sorties de débogage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage peut sembler être une tâche simple et banale, mais elle peut être un outil extrêmement utile pour les programmeurs. Non seulement elle aide à identifier et à résoudre les erreurs dans le code, mais elle peut également fournir des informations précieuses sur le fonctionnement de votre programme. Dans cet article, nous allons explorer en détail l'utilisation de l'impression de sortie de débogage en Gleam.

## Comment faire

L'impression de sortie de débogage en Gleam est très simple et peut être réalisée en utilisant la fonction `io.format`. Cette fonction prend deux arguments: un format de chaîne et une liste de valeurs à insérer dans le format. Voyons un exemple concret:

```
Gleam module debug_example

pub fn print_output(a) {
  io.format("La valeur de a est: {}\n", [a])
}

print_output(10)
```

Dans cet exemple, nous définissons une fonction nommée `print_output` qui prend un argument `a` et utilise `io.format` pour imprimer la valeur de `a` dans un format spécifique. En utilisant cela dans notre programme, nous pouvons facilement vérifier la valeur de n'importe quelle variable à n'importe quel moment.

## Plongée profonde

Il est également possible d'ajouter plus de détails à votre sortie de débogage en utilisant des options de formatage. Par exemple, vous pouvez ajouter des informations de type de données en utilisant `{:?}` et `{{:?}}` pour les paramètres et les valeurs respectivement. Vous pouvez également personnaliser les options en utilisant `{:,}`, `{;}` et `{:b}` pour les chaînes, les nombres de type `float` et `bool` respectivement. Cela peut être très utile lorsque vous avez besoin de plus d'informations sur les valeurs que vous imprimez.

## Voir aussi

- [Guide de formatage](https://gleam.run/book/tour/formatting.html)
- [Documentation de la fonction io.format](https://hexdocs.pm/gleam_stdlib/io.html#format-2)