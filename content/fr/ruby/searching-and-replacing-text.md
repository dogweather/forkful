---
title:    "Ruby: Recherche et remplacement de texte"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque l'on écrit du code, il est souvent nécessaire de rechercher et remplacer du texte dans nos fichiers. Cela peut être pour corriger des erreurs, modifier des noms de variables ou encore pour effectuer des changements sur plusieurs lignes en une seule fois. Heureusement, le langage de programmation Ruby nous permet de le faire assez facilement grâce à ses fonctionnalités de traitement de texte.

## Comment Faire

Supposons que nous ayons un fichier texte contenant plusieurs occurences du mot "hello" :
```
hello world
hello Ruby
hello programming
```
Pour remplacer toutes ces occurrences par "bonjour", nous pouvons utiliser la méthode `gsub` :
```Ruby
texte = "hello world
hello Ruby
hello programming"

texte.gsub!("hello", "bonjour")

puts texte 
```
Cela produirait la sortie suivante :
```
bonjour world
bonjour Ruby
bonjour programming
```
Comme vous pouvez le voir, toutes les occurrences de "hello" ont été remplacées par "bonjour". La méthode `gsub` prend également en compte la casse, donc si vous voulez remplacer également les mots en majuscules, utilisez la méthode `gsub!` sans le point d'exclamation.

Pour remplacer du texte dans un fichier existant, vous pouvez utiliser la méthode `File.read`, qui lira le contenu du fichier et le stockera dans une variable, puis vous pouvez utiliser la méthode `gsub` pour remplacer le texte, et enfin enregistrer les modifications avec la méthode `File.write`.

## Deep Dive

Même si la méthode `gsub` est très utile pour les remplacements simples de texte, il existe également des options plus avancées pour effectuer des recherches et remplacements plus spécifiques.

Par exemple, nous pouvons utiliser les Regex (expressions régulières) pour chercher des motifs de texte et les remplacer. Par exemple, si nous voulons remplacer tous les nombres dans une chaîne de caractères par "0", nous pouvons utiliser la fonction `gsub` suivante :
```Ruby
texte = "Il y a 3 chats et 5 chiens dans le parc"

texte.gsub!(/\d+/, "0")

puts texte
```
Et la sortie serait :
```
Il y a 0 chats et 0 chiens dans le parc
```
Nous pouvons également spécifier des options supplémentaires pour nos Regex, telles que les options de casse sensible/insensible.

## Voir Aussi

Si vous souhaitez en savoir plus sur la recherche et le remplacement de texte en Ruby, voici quelques ressources utiles :

- [La documentation officielle de la méthode `gsub'](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Un tutoriel sur l'utilisation de Regex en Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
- [Un article sur les bonnes pratiques de recherche et remplacement en Ruby](https://www.codewithjason.com/ruby-search-replace-text-files/)

Bon codage !