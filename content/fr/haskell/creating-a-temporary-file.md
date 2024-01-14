---
title:    "Haskell: Création d'un fichier temporaire"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Haskell

Si vous travaillez sur un projet Haskell qui nécessite de créer des fichiers temporaires, vous vous êtes probablement demandé pourquoi c'est nécessaire. Eh bien, la réponse est simple : les fichiers temporaires sont utiles pour stocker des données temporaires qui ne doivent pas être conservées de manière permanente.

## Comment faire

Pour créer un fichier temporaire en Haskell, vous pouvez utiliser la fonction `withTempFile` du module `System.IO.Temp`. Voici un exemple de code :

```Haskell
import System.IO.Temp
import System.IO
```

```Haskell
main = do
  withTempFile "fichiers_temporaires" "monfichier.txt" $ \tmpFile tmpHandle -> do
    hPutStrLn tmpHandle "Ceci est un fichier temporaire."
    hClose tmpHandle
    putStrLn "Fichier temporaire créé avec succès."
```

Ici, nous importons le module `System.IO.Temp` pour accéder à la fonction `withTempFile` et le module `System.IO` pour utiliser la fonction `hPutStrLn`. Ensuite, nous utilisons `withTempFile` en spécifiant le chemin d'accès et le nom du fichier temporaire que nous voulons créer. Nous utilisons également une fonction lambda pour écrire dans le fichier temporaire et le fermer une fois terminé.

Lorsque nous exécutons le code, nous devrions obtenir la sortie suivante :

```
Fichier temporaire créé avec succès.
```

## Plongée en profondeur

Maintenant que nous savons comment créer un fichier temporaire en Haskell, explorons un peu plus en détail comment cela fonctionne. Lorsque nous appelons la fonction `withTempFile`, elle effectue les opérations suivantes :

1. Elle génère un nom de fichier temporaire unique à l'aide du chemin d'accès et du nom que nous avons spécifiés.
2. Elle crée le fichier à l'aide de ce nom et le retourne sous forme de poignée de fichier (`Handle`).
3. Elle exécute l'action que nous avons spécifiée, en passant la poignée de fichier comme argument.
4. Une fois que l'action est terminée, elle ferme le fichier et le supprime.

Cela garantit que le fichier temporaire est toujours fermé et supprimé, même en cas d'erreur ou d'exception dans notre code.

## Voir aussi

- [Documentation sur la fonction `withTempFile`](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html#v:withTempFile)
- [Tutoriel sur la gestion des fichiers temporaires en Haskell](https://coderefinery.github.io/basic-haskell/files/)