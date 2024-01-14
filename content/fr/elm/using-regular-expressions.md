---
title:                "Elm: L'utilisation des expressions régulières"
simple_title:         "L'utilisation des expressions régulières"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Elm en herbe, vous avez probablement entendu parler des expressions régulières (ou *regular expressions* en anglais). Mais pourquoi voudriez-vous les utiliser dans votre code ?

Les expressions régulières sont des outils puissants pour manipuler et chercher des chaînes de caractères dans vos programmes. Elles sont particulièrement utiles lorsqu'il s'agit de valider des entrées utilisateur ou de filtrer des données. En les maîtrisant, vous pourrez simplifier et améliorer efficacement vos scripts Elm.

## Comment faire

Pour utiliser des expressions régulières en Elm, vous pouvez utiliser la bibliothèque `elm/regex` intégrée. Vous devez d'abord importer cette bibliothèque dans votre fichier en ajoutant la ligne suivante :

```Elm
import Regex
```

Ensuite, vous pouvez utiliser la fonction `Regex.contains` pour vérifier si une chaîne de caractères contient une expression régulière spécifique. Par exemple, si vous voulez vérifier si une adresse e-mail est valide, vous pouvez utiliser le code suivant :

```Elm
Regex.contains (Regex.regex "[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,63}") "monemail@elm.com"
```

Ce code renverra `True` car l'adresse e-mail est valide. Si vous voulez extraire une partie d'une chaîne de caractères, vous pouvez utiliser la fonction `Regex.find` et spécifier la partie que vous voulez extraire en utilisant des groupes de capture. Par exemple :

```Elm
Regex.find (Regex.regex "([A-Z0-9._%+-]+)@[A-Z0-9.-]+\.[A-Z]{2,63}") "monemail@elm.com"
```

Cela renverra `Just "monemail"` car nous avons spécifié que nous voulons extraire la partie avant le symbole "@".

## Plongée en profondeur

Si vous voulez en savoir plus sur les expressions régulières et leur syntaxe, il existe de nombreuses ressources en ligne, telles que [ce tutoriel sur les expressions régulières en français](https://www.creativejuiz.fr/blog/astuces/regex-pour-les-nuls-20-exemples-reguliers-utiles) ou [cette documentation sur les expressions régulières en Elm](https://package.elm-lang.org/packages/elm/regex/latest/Regex).

Il est important de noter que les expressions régulières peuvent être difficiles à comprendre et à déboguer, surtout pour les débutants. Il est donc recommandé de pratiquer et de tester vos expressions régulières avant de les utiliser dans votre code.

## Voir aussi

- [Documentation officielle Elm sur les expressions régulières](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Tutoriel sur les expressions régulières en français](https://www.creativejuiz.fr/blog/astuces/regex-pour-les-nuls-20-exemples-reguliers-utiles)
- [Différents outils pour tester et expérimenter avec des expressions régulières](https://www.creativejuiz.fr/blog/outils-tester-adn-texte-expressions-regulieres)