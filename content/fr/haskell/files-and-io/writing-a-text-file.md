---
title:                "Rédiger un fichier texte"
date:                  2024-02-03T19:28:01.040249-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédiger un fichier texte"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire dans un fichier texte en Haskell consiste à créer ou mettre à jour des fichiers avec du contenu textuel de manière programmatique. Les programmeurs font cela pour persister des données telles que les messages de journalisation, la sortie d'applications ou pour stocker du contenu généré par l’utilisateur, ce qui en fait une tâche fondamentale pour les applications nécessitant la persistance des données ou la journalisation.

## Comment faire :

Le Prelude standard d'Haskell fournit un support élémentaire pour l'écriture dans des fichiers en utilisant les fonctions `writeFile` et `appendFile` du module `System.IO`. Voici un exemple de base pour créer un nouveau fichier (ou écraser un existant) puis ajouter du texte à un fichier.

```haskell
import System.IO

-- Écrire dans un fichier, en l'écrasant s'il existe
main :: IO ()
main = do
  writeFile "example.txt" "Ceci est la première ligne.\n"
  appendFile "example.txt" "Ceci est la deuxième ligne.\n"
```

Lorsque vous exécutez ce programme, il crée (ou vide) `example.txt` et écrit "Ceci est la première ligne." suivi de "Ceci est la deuxième ligne." à la ligne suivante.

Pour une gestion des fichiers plus avancée, les programmeurs Haskell se tournent souvent vers le paquet `text` pour un traitement efficace des chaînes de caractères et le paquet `bytestring` pour la gestion des données binaires. Voici comment utiliser le paquet `text` pour l'IO de fichiers :

D'abord, vous devez ajouter `text` aux dépendances de votre projet. Ensuite, vous pouvez l'utiliser comme suit :

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Écrire dans un fichier en utilisant le paquet text
main :: IO ()
main = do
  let content = T.pack "Utilisation du paquet text pour une meilleure performance.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "Ajout de la deuxième ligne.\n"
```

Dans cet extrait, `T.pack` convertit une `String` ordinaire en le type `Text`, qui est plus efficace. `TIO.writeFile` et `TIO.appendFile` sont les équivalents dans le paquet `text` pour écrire et ajouter à des fichiers, respectivement.

L'exécution de ce code résultera en un fichier nommé `textExample.txt` avec deux lignes de texte, démontrant à la fois les capacités de création et d'ajout en utilisant la bibliothèque `text` avancée pour une meilleure performance et une meilleure capacité de gestion du texte Unicode.
