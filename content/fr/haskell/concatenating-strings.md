---
title:                "Concaténation de chaînes"
html_title:           "Haskell: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi le faire?
La concaténation de chaînes de caractères est le fait de fusionner deux ou plusieurs chaînes de caractères en une seule. Les programmeurs font cela pour faciliter la manipulation et la présentation de données, ainsi que pour créer des chaînes de caractères plus complexes à partir de plusieurs petites chaînes.

## Comment faire:
Voici un exemple de code Haskell pour concaténer deux chaînes de caractères:

```Haskell
concatenation :: String -> String -> String
concatenation x y = x ++ y

main = do
  let chaine1 = "Bonjour"
  let chaine2 = "monde!"
  let chaine_concatenee = concatenation chaine1 chaine2
  putStrLn chaine_concatenee

-- Output: "Bonjourmonde!"
```
Dans cet exemple, nous définissons d'abord une fonction de concaténation qui prend deux chaînes de caractères en entrée et retourne la concaténation des deux. Ensuite, dans la fonction principale, nous définissons deux chaînes de caractères et utilisons notre fonction de concaténation pour les fusionner. Enfin, nous utilisons la fonction `putStrLn` pour afficher la chaîne concaténée à l'écran.

## Plongée en profondeur:
La concaténation de chaînes de caractères est une opération courante en programmation, et elle est disponible dans de nombreux langages de programmation en plus de Haskell, comme JavaScript et Python. Dans certains langages, la concaténation utilise le symbole `+` au lieu de `++` comme dans Haskell.

Il existe également d'autres façons de concaténer des chaînes de caractères, telles que l'utilisation de la fonction `concat` en Haskell pour fusionner une liste de chaînes de caractères en une seule chaîne. Il existe également des bibliothèques qui offrent des fonctionnalités plus avancées pour manipuler et formater des chaînes de caractères, telles que la célèbre bibliothèque `Text` en Haskell.

## Voir aussi:
- [Documentation officielle de concaténation en Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html#g:22)
- [Documentation de la bibliothèque `Text` en Haskell](https://hackage.haskell.org/package/text/docs/Data-Text.html#v:concat)
- [Explications sur la concaténation en JavaScript](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [Tutoriel sur la manipulation de chaînes de caractères en Python](https://www.programiz.com/python-programming/string)