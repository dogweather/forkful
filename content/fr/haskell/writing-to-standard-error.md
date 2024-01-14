---
title:                "Haskell: Écrire vers l'erreur standard"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire vers l'erreur standard en Haskell?

Écrire vers l'erreur standard en Haskell peut être utile pour afficher des messages d'erreur ou de débogage dans la console, plutôt que dans la sortie standard où ils peuvent aller inaperçus.

## Comment procéder?

Voici un exemple de code Haskell montrant comment écrire vers l'erreur standard en utilisant la bibliothèque `System.IO`:

```Haskell
import System.IO

main = do
   hPutStrLn stderr "Ceci est un message d'erreur."
```

Lorsque nous exécutons ce code, le message d'erreur sera affiché dans la console au lieu d'être redirigé vers la sortie standard. Voici l'exemple de sortie que nous obtiendrons:

```
Ceci est un message d'erreur.
```

## Plongeons plus en profondeur

Il est également possible de personnaliser le message d'erreur en utilisant des formats de chaîne et en passant des variables en tant qu'arguments de la fonction `hPutStrLn`. Voici un exemple de code pour illustrer cela:

```Haskell
import System.IO

main = do
   let num = 42
   hPutStrLn stderr $ "Le nombre " ++ show num ++ " est un élément essentiel pour la réponse ultime de l'univers."
```

Dans cet exemple, nous utilisons la fonction `show` pour convertir la variable `num` de type `Int` en une chaîne de caractères afin de concaténer correctement notre message. Voici à quoi ressemblera la sortie:

```
Le nombre 42 est un élément essentiel pour la réponse ultime de l'univers.
```

# Voir aussi

- [Documentation sur la bibliothèque System.IO en Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Tutoriel Haskell pour débutants](https://www.tutorialspoint.com/haskell/index.htm)