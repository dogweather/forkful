---
title:    "Haskell: Concaténation de chaînes"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation en Haskell, il est fréquent d'avoir à concaténer des chaînes de caractères, c'est-à-dire de les combiner en une seule chaîne. Cette opération est très utile dans de nombreux cas, comme par exemple lors de la création de messages ou de rapports.

## Comment faire

La concaténation de chaînes de caractères en Haskell est très simple grâce à la fonction `++`. Lorsque vous concaténez deux chaînes, la première sera placée avant la seconde. Voici un exemple de code :

```Haskell
concatenate :: String -> String -> String
concatenate str1 str2 = str1 ++ str2

-- Output : "Bonjour le monde!"
concatenate "Bonjour" " le monde!"
```

Vous pouvez également concaténer plusieurs chaînes en une seule en utilisant la fonction `concat`. Celle-ci prend en argument une liste de chaînes et les combine en une seule. Voici un autre exemple de code :

```Haskell
multipleConcat :: [String] -> String
multipleConcat strings = concat strings

-- Output : "Je suis un programmeur en Haskell"
multipleConcat ["Je ", "suis ", "un ", "programmeur ", "en ", "Haskell"]
```

## Plongée en profondeur

En Haskell, les chaînes de caractères sont en fait des listes de caractères. La fonction `++` fonctionne en prenant la première liste et en y ajoutant la seconde à la fin. Cela signifie que si vous concaténez une chaîne avec un seul caractère, celui-ci sera également ajouté à la fin. Par exemple :

```Haskell
-- Output : "Hello world!"
concatenate "Hello " "w" ++ "orld!"
```

Il est également possible de concaténer des chaînes non alphabétiques, comme des nombres. Cependant, il est important de noter que la fonction `concat` fonctionne uniquement avec des listes de chaînes, il faut donc convertir les nombres en chaînes avant de les concaténer. Par exemple :

```Haskell
-- Output : "Le résultat est : 8"
concatenate "Le résultat est : " (show (6+2))
```

Enfin, il existe d'autres fonctions utiles pour manipuler les chaînes de caractères en Haskell, comme `unwords` qui concatène une liste de mots en une seule chaîne avec des espaces entre chacun, ou encore `lines` qui sépare une chaîne en plusieurs lignes en fonction des retours à la ligne. Ces fonctions peuvent être très pratiques pour manipuler des données ou des fichiers textes.

## Voir aussi

- [Documentation officielle de la concaténation en Haskell](https://www.haskell.org/tutorial/strings.html#function-)
- [Tutoriel sur les chaînes de caractères en Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Exemple de manipulation de chaînes de caractères en Haskell](https://wiki.haskell.org/More_about_lists#Strings)