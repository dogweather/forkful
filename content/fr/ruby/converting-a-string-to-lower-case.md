---
title:                "Ruby: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi 

La conversion d'une chaîne de caractères en minuscules est une tâche courante en programmation Ruby, car elle permet de normaliser les données et de faciliter leur traitement. Cela peut être utile pour le traitement de données utilisateur ou pour la comparaison de chaînes de caractères. 

## Comment faire 

Pour convertir une chaîne de caractères en minuscules en Ruby, vous pouvez utiliser la méthode `downcase`. Voici un exemple de code montrant comment utiliser cette méthode : 

```Ruby 
string = "Bonjour Monde" 
puts string.downcase 
``` 

La sortie de ce code sera : *bonjour monde*. Comme vous pouvez le voir, la méthode `downcase` a converti toutes les lettres en minuscules. 

## Plongée en profondeur 

En plongeant un peu plus dans le sujet, il est important de noter que la méthode `downcase` ne convertit que les lettres de la langue anglaise en minuscules. Si vous travaillez avec des caractères accentués ou des caractères de langue étrangère, vous devrez utiliser la méthode `downcase!` pour une conversion correcte. De plus, si vous souhaitez ignorer la casse lors de la comparaison de chaînes de caractères, vous pouvez utiliser la méthode `casecmp`, qui renvoie un nombre négatif si la première chaîne est inférieure à la seconde en termes de tri de dictionnaire, 0 si les chaînes sont égales et un nombre positif si la première chaîne est supérieure à la seconde. 

# Voir aussi 

- [Documentation officielle Ruby sur la méthode `downcase`](https://ruby-doc.org/core-2.6.3/String.html#method-i-downcase) 
- [Documentation officielle Ruby sur la méthode `casecmp`](https://ruby-doc.org/core-2.6.3/String.html#method-i-casecmp)