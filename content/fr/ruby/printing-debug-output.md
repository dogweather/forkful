---
title:                "Ruby: Sortie de débogage à l'impression"
simple_title:         "Sortie de débogage à l'impression"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage, également connue sous le nom de "printing debug output" en anglais, est une pratique courante pour les développeurs de Ruby. Elle consiste à afficher des informations sur l'exécution d'un programme à des fins de débogage. Cela peut sembler être une étape évidente pour certains, mais cela peut en réalité être très utile pour résoudre des erreurs dans le code.

## Comment procéder

Pour imprimer une sortie de débogage dans votre code Ruby, il suffit d'utiliser la méthode ```puts```. Par exemple, si nous voulons vérifier la valeur d'une variable appelée "nom", nous pouvons utiliser le code suivant :

```
nom = "Jean"
puts "La valeur de la variable nom est : #{nom}"
```

Cela affichera dans la console la phrase "La valeur de la variable nom est : Jean". Vous pouvez également utiliser la méthode ```p```, qui a un comportement légèrement différent, en imprimant la valeur avec des guillemets.

## Approfondissement

Il est important de noter que l'impression de sortie de débogage peut également être utilisée pour afficher des informations sur des objets complexes, tels que des tableaux ou des hashs. Vous pouvez également utiliser des options supplémentaires telles que la méthode ```inspect``` pour afficher des informations détaillées sur un objet.

Il est recommandé d'utiliser la création de "points de contrôle" dans votre code, en imprimant des informations à des endroits clés pour mieux comprendre le flux de votre programme et détecter les erreurs. Cependant, il est important de noter que le nettoyage de ces commandes d'impression lors de la mise en production de votre code est essentiel, afin de maintenir la performance et la sécurité de votre application.

## Voir aussi

- https://www.sitepoint.com/printing-debug-output-in-ruby/
- https://www.rubyguides.com/2019/06/ruby-print-vs-puts-vs-p/
- https://www.ruby-lang.org/en/documentation/quickstart/1/