---
title:                "Ruby: Extraction de sous-chaînes"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

Pourquoi :

L'extraction de sous-chaînes est une compétence essentielle pour tout programmeur Ruby. Cela vous permet de découper une chaîne de caractères en morceaux plus petits, ce qui est utile pour manipuler et analyser des données. Dans cet article, nous allons vous montrer comment extraire efficacement des sous-chaînes en utilisant Ruby.

Comment faire :

Pour extraire une sous-chaîne en Ruby, nous utilisons la méthode `slice` ou `[]`. Voici un exemple de code :

```Ruby
string = "Bonjour à tous!"
substring = string.slice(8, 3)
puts substring
```

La sortie de ce code sera "à to". Nous utilisons la méthode `slice` en spécifiant l'index de départ et le nombre de caractères à extraire.

Une autre façon d'extraire une sous-chaîne est d'utiliser la notation des crochets, qui ressemble à ceci :

```Ruby
string = "Bonjour à tous!"
substring = string[8, 3]
puts substring
```

La sortie sera la même que dans l'exemple précédent. La notation des crochets est très similaire à l'utilisation de la méthode `slice`, sauf que nous n'avons pas besoin d'écrire le mot-clé `slice` avant les crochets.

En plus des méthodes `slice` et `[]`, il existe d'autres méthodes utiles pour extraire des sous-chaînes en Ruby, telles que `lstrip` pour supprimer les espaces vides à gauche de la sous-chaîne et `chomp` pour supprimer les caractères de fin (comme les sauts de ligne).

Plongée en profondeur :

Pourquoi est-il important de comprendre comment extraire des sous-chaînes en Ruby ? Eh bien, cela peut vous aider dans de nombreux cas de programmation, tels que la manipulation de texte, l'analyse de données et la validation de saisie utilisateur.

De plus, la maîtrise de ces techniques peut vous aider à écrire un code plus propre et plus efficace, en utilisant les méthodes appropriées pour chaque situation.

Enfin, en apprenant à extraire des sous-chaînes, vous serez en mesure de mieux comprendre comment fonctionnent les chaînes de caractères en Ruby, ce qui est essentiel pour devenir un programmeur Ruby compétent.

Voir aussi :

- [Documentation officielle Ruby sur les méthodes de chaînes](https://ruby-doc.org/core-2.7.1/String.html)
- [Tutoriel sur les manipulations de chaînes en Ruby](https://www.rubyguides.com/2019/12/ruby-string-manipulation/)
- [Article sur l'importance de bien comprendre les chaînes de caractères en programmation](https://blog.rubyguides.com/ruby-strings/)