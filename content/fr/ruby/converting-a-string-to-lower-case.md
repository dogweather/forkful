---
title:    "Ruby: Convertir une chaîne en minuscules"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Les développeurs en Ruby peuvent être confrontés à un problème courant où ils ont besoin de convertir une chaîne de caractères en minuscules. Cela peut être nécessaire pour des raisons telles que la comparaison de chaînes en ignorant la casse ou pour l'affichage cohérent de texte dans une application.

## Comment Faire

Pour convertir une chaîne de caractères en minuscules en Ruby, nous pouvons utiliser la méthode `downcase`. Voici un exemple de code utilisant cette méthode :

```Ruby
string = "Bonjour Le Monde"
puts string.downcase
```

La sortie de ce code sera "bonjour le monde". Nous pouvons également utiliser cette méthode pour comparer des chaînes en ignorant la casse, comme dans l'exemple suivant :

```Ruby
string1 = "Bonjour"
string2 = "BONJOUR"

puts string1.downcase == string2.downcase
```

La sortie de ce code sera `true` car les deux chaînes sont égales lorsqu'elles sont converties en minuscules.

## Deep Dive

La méthode `downcase` utilise les règles de casse définies par l'encodage de caractères de la chaîne de caractères. Cela signifie que si vous travaillez avec des caractères non ascii tels que les caractères accentués en français, la conversion en minuscules peut parfois donner des résultats inattendus.

Un autre point important à noter est que la méthode `downcase` ne modifie pas la chaîne de caractères d'origine, elle renvoie plutôt une nouvelle chaîne avec les modifications de cas.

## Voir Aussi

- [Documentation officielle de la méthode `downcase`](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Page de référence sur les méthodes de conversion de chaînes en Ruby](https://www.rubyguides.com/2018/10/ruby-string-methods/)
- [Article expliquant les règles de casse en Ruby](https://www.rubyguides.com/2018/10/string-case-methods/)