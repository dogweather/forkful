---
title:                "Retirer les guillemets d'une chaîne"
aliases:
- /fr/ruby/removing-quotes-from-a-string/
date:                  2024-01-26T03:41:23.184999-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Supprimer les guillemets d'une chaîne signifie retirer ces marques de citation doubles ou simples qui entourent les valeurs de texte. Les programmeurs font souvent cela pour nettoyer les entrées des utilisateurs, assurer la cohérence dans le traitement des données, ou préparer des données pour des systèmes qui pourraient être confus par ces caractères supplémentaires.

## Comment faire :
Ruby possède quelques astuces sympas pour couper ces guillemets ennuyeux. Vous pouvez utiliser les méthodes `gsub` ou `delete` pour accomplir cette tâche. Voici du code à méditer :

```ruby
# Utilisation de gsub pour supprimer les guillemets doubles et simples
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Sortie : Say hello to my little friend!

# Si vous savez que vous ne traiterez qu'un seul type de guillemet
single_quoted_string = "'Reste un moment et écoute !'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Sortie : Reste un moment et écoute !
```

## Plongée en profondeur
L'histoire des guillemets remonte aux premiers jours de la programmation, où ils servaient souvent de délimiteurs de chaînes. De nos jours, comme à l'époque, vous pourriez vous retrouver à devoir supprimer ces caractères de citation quand ils ne sont pas nécessaires ou lorsqu'ils pourraient interférer avec le stockage et la manipulation des données.

Nous avons parlé de `gsub` et de `delete`, mais il existe également d'autres méthodes, comme `tr` ou `tr_s`, qui vous donnent un peu plus de contrôle ou peuvent gérer certains cas d'utilisation différents :

```ruby
# tr peut également supprimer les guillemets
double_quoted_string = "\"Faire ou ne pas faire, il n’y a pas d’essai.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Sortie : Faire ou ne pas faire, il n’y a pas d’essai.
```

Rappelez-vous, chacune de ces méthodes a ses cas d'utilisation. `gsub` est plus puissant lorsque vous avez affaire à des motifs complexes ou à de multiples substitutions. `delete` et `tr` fonctionnent à merveille pour des suppressions de caractères simples et directes.

## Voir aussi
Pour des lectures supplémentaires, et pour voir ces méthodes en action dans des bases de code plus importantes, consultez :
- La documentation Ruby pour [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), et [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas propose un excellent [ensemble d'exercices sur les chaînes de caractères](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), qui inclut le travail avec les guillemets.
- Les discussions sur Stack Overflow concernant la [manipulation de chaînes](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) fournissent des problèmes et solutions du monde réel de la part de Rubyistes.
