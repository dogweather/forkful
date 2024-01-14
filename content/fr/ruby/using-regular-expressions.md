---
title:                "Ruby: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Utiliser des expressions régulières peut sembler intimidant au premier abord, mais c'est en fait un outil puissant pour manipuler et analyser des chaînes de caractères en Ruby. Que vous cherchiez à valider des données entrées par l'utilisateur, à extraire des informations d'un fichier texte ou même à remplacer des mots dans une chaîne, les expressions régulières peuvent vous faire gagner un temps précieux et rendre votre code plus élégant.

## Comment faire

Pour utiliser les expressions régulières en Ruby, vous devez d'abord les encapsuler entre deux slashs (`/expression/`) dans une variable ou directement dans une méthode comme `gsub` ou `match`. Ensuite, vous pouvez utiliser différents symboles avant ou après l'expression pour spécifier ce que vous cherchez à faire.

Par exemple, si vous voulez chercher toutes les occurrences d'une lettre dans une chaîne de caractères, vous pouvez utiliser le symbole `.` pour représenter n'importe quel caractère, suivi du symbole `*` qui signifie "0 ou plus". Voici un exemple de code :

```Ruby
texte = "Bonjour tout le monde!"
resultat = texte.scan(/to\*/) # trouve toutes les lettres "to" dans la chaîne
puts resultat # affiche ["tout"]
```

Vous pouvez également utiliser des symboles spéciaux pour rechercher des ensembles de caractères spécifiques, comme `[aeiou]` pour toutes les voyelles ou `[A-Z]` pour toutes les lettres majuscules. Pour plus d'exemples et de détails, consultez la section "Voir aussi" ci-dessous.

## Plongée en profondeur

Les expressions régulières peuvent sembler complexes au début, mais elles peuvent être très utiles pour des tâches plus avancées. Par exemple, vous pouvez utiliser des parenthèses pour capturer les parties spécifiques d'une chaîne que vous souhaitez extraire. Vous pouvez également utiliser les symboles `+`, `?` ou `{n, m}` pour spécifier des occurrences multiples, facultatives ou d'un nombre précis.

En plus des techniques de correspondance, les expressions régulières en Ruby ont également des méthodes utiles telles que `sub` qui remplace la première occurrence correspondante ou `gsub` qui remplace toutes les occurrences correspondantes. Il existe également des options comme l'utilisation de modificateurs d'expression régulière pour ignorer la casse ou utiliser des expressions régulières multilignes.

## Voir aussi

- [Documentation Ruby pour les expressions régulières] (https://ruby-doc.org/core/Regexp.html)
- [Un guide complet des expressions régulières en Ruby] (https://www.regular-expressions.info/ruby.html)
- [Rubular - tester vos expressions régulières en temps réel] (https://rubular.com/)