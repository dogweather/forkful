---
title:                "Imprimer la sortie de débogage"
html_title:           "Fish Shell: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez probablement rencontré des erreurs ou des bugs pendant votre approche de la programmation dans Fish Shell. Le moyen le plus simple et le plus efficace de comprendre ce qui se passe est d'imprimer des sorties de débogage pour voir l'état de votre code à différents points d'exécution. Cela peut vous aider à localiser rapidement et à résoudre les problèmes dans votre script.

## Comment faire

Voici comment vous pouvez afficher des sorties de débogage en utilisant Fish Shell:

```
set debug_mode true
set status_verbose true

# Votre code ici

set debug_mode false
set status_verbose false
```

En définissant la variable ```debug_mode``` sur ```true```, vous activerez le mode de débogage dans Fish Shell. Cela signifie que toutes les sorties de débogage seront imprimées à l'écran. De plus, vous pouvez également définir la variable ```status_verbose``` sur ```true``` pour obtenir des informations supplémentaires sur l'état de votre code.

Vous pouvez également personnaliser les sorties de débogage en utilisant la commande ```echo``` de Fish Shell. Par exemple:

```
set foo "bar"
echo "La valeur de la variable foo est: $foo"
```

Cela imprimera à l'écran: ```La valeur de la variable foo est: bar``` et vous donnera un aperçu de la valeur de la variable à un moment précis dans votre code.

## Plongée en profondeur

En plus des exemples ci-dessus, Fish Shell offre de nombreuses autres options pour afficher des sorties de débogage, telles que l'utilisation de la commande ```fish_trace```, qui imprimera tous les appels de fonctions et les commandes exécutées, ou encore l'utilisation de la variable ```status```, qui contient des informations sur la dernière commande exécutée.

N'hésitez pas à explorer davantage les nombreuses fonctions de débogage offertes par Fish Shell pour faciliter votre processus de développement.

## Voir aussi

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Un tutoriel sur la détection et la correction des erreurs dans Fish Shell](https://scriptingosx.com/2017/10/fixing-errors-with-fish-shell/)