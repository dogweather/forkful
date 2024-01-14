---
title:    "Ruby: Trouver la longueur d'une chaîne de caractères"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles on pourrait vouloir trouver la longueur d'une chaîne de caractères en programmation. Par exemple, cela peut être utile pour vérifier la validité d'une entrée utilisateur, calculer des statistiques sur des données textuelles ou simplement pour satisfaire votre curiosité en tant que développeur.

## Comment faire

En Ruby, pour trouver la longueur d'une chaîne de caractères, il suffit d'utiliser la méthode `length` comme ceci :

```Ruby
"Bonjour".length # renvoie 7
```

Vous pouvez également utiliser la méthode `size` qui renvoie également la longueur d'une chaîne :

```Ruby
"Bonjour".size # renvoie 7
```

Il est important de noter que ces méthodes renvoient également la longueur des espaces et des caractères spéciaux.

## Une plongée plus profonde

Il est possible d'accéder directement à la longueur d'une chaîne de caractères en accédant à sa propriété `length`. Cela peut être utile lorsque vous devez vérifier la longueur d'une chaîne avant de l'utiliser.

Par exemple, si vous voulez vérifier qu'un mot de passe a au moins 8 caractères, vous pouvez utiliser une structure conditionnelle comme ceci :

```Ruby
password = gets.chomp # demande à l'utilisateur de saisir un mot de passe
if password.length < 8 # vérifie si la longueur du mot de passe est inférieure à 8
  puts "Le mot de passe doit contenir au moins 8 caractères."
else
  puts "Le mot de passe est valide."
end
```

## Voir aussi

Pour en savoir plus sur les méthodes `length` et `size` en Ruby, consultez la documentation officielle [ici](https://ruby-doc.org/core-2.7.1/String.html#method-i-length) et [ici](https://ruby-doc.org/core-2.7.1/String.html#method-i-size). Vous pouvez également explorer d'autres fonctionnalités utiles de Ruby en parcourant les liens suivants :

- [Manipulation de chaînes de caractères avec Ruby](https://www.rubyguides.com/2015/05/working-with-strings-in-ruby/)
- [Tutoriel Ruby pour débutants](https://www.codecademy.com/learn/learn-ruby)
- [10 astuces Ruby pour les développeurs](https://www.rubygarage.org/blog/ruby-tricks-for-productivity)

Merci d'avoir lu et à bientôt pour plus de contenu sur la programmation en Ruby !