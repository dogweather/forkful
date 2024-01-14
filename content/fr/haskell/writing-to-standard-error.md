---
title:    "Haskell: Écriture sur le flux d'erreur standard"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi Écrire au Flot d'Erreur en Haskell 

L'écriture au flot d'erreur (standard error) est une pratique courante en programmation Haskell. Elle nous permet d'afficher des erreurs et des informations de débogage directement dans notre terminal, ce qui peut grandement faciliter le processus de débogage d'un programme. Dans cet article, nous allons explorer ce qu'est le flot d'erreur, pourquoi il est utile et comment l'utiliser dans nos programmes Haskell.

## Comment Faire 

Pour écrire au flot d'erreur en Haskell, nous pouvons utiliser la fonction `hPutStrLn` du module `System.IO`. Cette fonction prend en paramètre un objet `Handle` et une chaîne de caractères, et écrit cette chaîne dans le flot d'erreur associé à ce `Handle`. 

Voici un exemple de code qui écrit un message d'erreur dans le flot d'erreur et termine immédiatement le programme :

```
hPutStrLn stderr "Erreur : Liste vide."
```

Si nous exécutons ce code dans notre terminal, nous verrons le message d'erreur s'afficher, et le programme s'arrêtera. Nous pouvons également utiliser cette fonction pour afficher des informations de débogage :

```
let x = 10
hPutStrLn stderr $ "La valeur de x est : " ++ show x
```

Ce code écrit dans le flot d'erreur : "La valeur de x est : 10". Nous pouvons ainsi afficher des valeurs de variables ou des messages de débogage pour nous aider à comprendre le comportement de notre programme.

## Plongée en Profondeur 

Le flot d'erreur en Haskell est associé à un `Handle` spécifique, `stderr`, qui représente le flot d'erreur standard du système. Il existe également un autre `Handle` appelé `stdout`, qui représente le flot de sortie standard.

Il est important de noter que les messages écrits dans le flot d'erreur ne sont pas redirigés avec les autres sorties standards lorsqu'on les redirige vers un fichier. Cela signifie que si nous écrivons `myProgram > output.txt`, les messages écrits dans le flot d'erreur ne seront pas inclus dans le fichier `output.txt`.

Il est également possible de rediriger le flot d'erreur vers un autre `Handle` en utilisant la fonction `hDup2`. Cela peut être utile si nous voulons gérer les erreurs de différentes manières selon le contexte de notre programme.

## Voir Aussi 

- [La documentation officielle de Haskell sur les entrées/sorties](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [Un article sur le flot d'erreur en Haskell](https://wiki.haskell.org/Introduction_to_IO#Standard_And_Error_IO) 
- [Un tutoriel complet sur les entrées/sorties en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#appendix-io)