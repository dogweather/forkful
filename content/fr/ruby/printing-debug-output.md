---
title:    "Ruby: Afficher la sortie de débogage"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous développez des applications en Ruby, il arrive souvent que vous rencontriez des erreurs ou des bugs. Dans ces cas-là, il est important de comprendre ce qui se passe dans votre code pour pouvoir le corriger efficacement. C'est là qu'intervient l'impression de sortie de débogage.

## Comment faire

La manière la plus simple de comprendre ce qui se passe dans votre code est d'imprimer des messages de débogage à certains endroits clés de votre programme. Pour ce faire, vous pouvez utiliser la méthode `puts` en y ajoutant des informations que vous souhaitez afficher. Par exemple :

```Ruby
puts "Début du programme."

# Votre code ici

puts "Fin du programme."
```

Cela vous permettra de voir dans votre terminal les différentes étapes de votre programme et ainsi mieux comprendre ce qui se passe.

## Plongée Profonde

L'impression de sortie de débogage peut également être utilisée pour afficher des valeurs de variables à un moment précis de l'exécution de votre code. Par exemple :

```Ruby
age = 25
puts "L'âge est de #{age} ans."
```

Cela sera particulièrement utile pour trouver la cause d'une erreur liée à une variable.

Vous pouvez également utiliser des conditions pour afficher différents messages de débogage en fonction de certaines situations :

```Ruby
if condition
  puts "Condition validée."
else
  puts "Condition non valide."
end
```

## Voir aussi

Si vous souhaitez en savoir plus sur l'impression de sortie de débogage, voici quelques liens utiles :

- [Ruby Doc - puts](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-puts)
- [Ruby Guides - Debugging](https://www.rubyguides.com/2019/09/ruby-debugging/)
- [One Month - Debugging in Ruby](https://onemonth.com/courses/ruby/steps/debugging)