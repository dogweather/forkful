---
title:    "Haskell: Écriture d'un fichier texte"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

### Pourquoi

Ecrire un fichier texte peut sembler banal et sans intérêt, mais c'est en réalité une compétence très importante pour tout programmeur ou programmeuse. En utilisant le langage de programmation Haskell, vous pouvez créer des fichiers texte qui sont faciles à lire pour les machines, mais aussi pour les êtres humains.

### Comment faire

Tout d'abord, vous aurez besoin de quelques connaissances de base en Haskell. Ensuite, vous pouvez suivre ces étapes simples pour écrire un fichier texte :

1. Importez le module `System.IO` dans votre programme.
2. Créez un nouveau fichier texte en utilisant la fonction `openFile`, en spécifiant le chemin d'accès et le mode d'ouverture (`WriteMode` pour créer un nouveau fichier ou `AppendMode` pour ajouter du contenu à un fichier existant).
3. Utilisez la fonction `hPutStrLn` pour écrire une chaîne de caractères dans le fichier.
4. Enfin, n'oubliez pas de fermer le fichier en utilisant la fonction `hClose`.

```Haskell
import System.IO

main = do
    -- Crée un nouveau fichier texte
    let filePath = "mon_fichier.txt"
    handle <- openFile filePath WriteMode
    -- Ajoute du contenu au fichier
    hPutStrLn handle "Bonjour tout le monde !"
    hPutStrLn handle "Je suis un fichier texte écrit en Haskell."
    -- Ferme le fichier
    hClose handle
```

Lorsque vous exécutez ce code, un nouveau fichier texte nommé `mon_fichier.txt` sera créé avec les deux phrases écrites à l'intérieur.

### Plongée en profondeur

Maintenant que vous savez comment écrire un fichier texte en Haskell, vous pouvez aller plus loin et explorer toutes les options disponibles. Par exemple, vous pouvez utiliser la fonction `hPrint` pour écrire des valeurs de différents types (entiers, flottants, booléens, etc.) dans le fichier. Vous pouvez également utiliser des fonctions de manipulation de chaînes de caractères pour créer des fichiers texte plus complexes.

### Voir aussi

- [Documentation du module System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutoriel Haskell pour débutants](https://haskell-lang.org/tutorial/getting-started)
- [Apprendre Haskell en une journée](https://wiki.haskell.org/Learning_Haskell_in_1_Day)