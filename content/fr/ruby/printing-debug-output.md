---
title:    "Ruby: Affichage du débogage"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression d'un débogage est un outil précieux pour les programmeurs. Cela permet de suivre pas à pas l'exécution du code et d'identifier les erreurs plus facilement. Cela peut également être utilisé pour comprendre le fonctionnement d'une partie spécifique du code.

## Comment faire

Il existe plusieurs façons d'imprimer un débogage en Ruby, mais la méthode la plus courante est d'utiliser la méthode `puts`. Cette méthode permet d'imprimer un message sur la console, ce qui est utile pour vérifier les valeurs des variables à différents points du code.

```Ruby
puts "Débogage: la valeur de x est #{x}"
```

L'utilisation de la méthode `p` est également courante car elle imprime directement la valeur de la variable sans la modifier.

```Ruby
p x # imprime la valeur actuelle de x
```

Une autre méthode utile est `pp`, qui imprime un objet de manière plus lisible en le séparant par des sauts de ligne.

```Ruby
pp hash # imprime le contenu du hash sur plusieurs lignes
```

## Plongeons plus en profondeur

L'impression d'un débogage peut également être utilisée pour afficher des informations sur l'exécution du code, telles que le temps écoulé entre certaines étapes ou le nombre de fois qu'une boucle a été exécutée.

```Ruby
start_time = Time.now # sauvegarde le temps actuel
# Code à déboguer
puts "Temps d'exécution: #{Time.now - start_time}s" # imprime le temps écoulé
```

Il est également possible d'imprimer des informations spécifiques à certaines parties du code, en utilisant des conditions pour contrôler l'impression.

```Ruby
# Code à déboguer
if x > 10
  puts "Débogage: x est supérieur à 10"
end
```

## Voir également

- [Documentation Ruby: Debugging](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-puts)
- [Rubrique de débogage dans le tutoriel Ruby](https://www.ruby-lang.org/fr/documentation/quickstart/)
- [Blog sur le débogage en Ruby](https://www.rubyguides.com/2019/04/ruby-debugging/)