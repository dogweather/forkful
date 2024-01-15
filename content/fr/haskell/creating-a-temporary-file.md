---
title:                "Création d'un fichier temporaire"
html_title:           "Haskell: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi

De temps en temps, lorsque vous programmez en Haskell, vous pourriez avoir besoin de créer un fichier temporaire pour stocker des données ou effectuer des opérations temporaires. La création de fichiers temporaires peut également être utile pour tester ou déboguer du code.

# Comment faire

La création d'un fichier temporaire en Haskell est très simple. Tout d'abord, nous devons importer un module appelé "System.IO.Temp" qui nous permet de travailler avec des fichiers temporaires. Ensuite, nous pouvons utiliser la fonction "withSystemTempFile" pour créer un fichier temporaire et y écrire des données. Voici un exemple de code pour créer un fichier temporaire et y écrire le texte "Bonjour Haskell !":

```Haskell
import System.IO.Temp

main = withSystemTempFile "example.txt" $ \path handle -> do
  hPutStr handle "Bonjour Haskell !"
  putStrLn $ "Fichier temporaire créé : " ++ path
```

Notez que la fonction "withSystemTempFile" prend deux arguments. Le premier est le nom que vous souhaitez donner au fichier temporaire et le deuxième est une fonction qui manipule le fichier temporaire. Dans cet exemple, nous utilisons une fonction lambda pour écrire des données dans le fichier.

Lorsque nous exécutons ce code, nous devrions voir le message "Fichier temporaire créé : /tmp/example.txt" s'afficher dans la console. Vous pouvez ensuite vérifier le contenu du fichier en utilisant la fonction "readFile" comme ceci :

```Haskell
main = withSystemTempFile "example.txt" $ \path handle -> do
  hPutStr handle "Bonjour Haskell !"
  content <- readFile path
  putStrLn $ "Le contenu du fichier temporaire est : " ++ content
```

# Plongée profonde

La fonction "withSystemTempFile" que nous avons utilisée prend en charge la création de fichiers temporaires de manière sûre et efficace. Elle crée un fichier temporaire dans le répertoire système approprié et s'assure également que le fichier est supprimé une fois que la fonction est terminée. Cela est particulièrement utile si votre programme est interrompu ou s'il se termine avant d'avoir supprimé le fichier temporaire.

En plus de la fonction "withSystemTempFile", le module "System.IO.Temp" offre également d'autres fonctions utiles pour travailler avec des fichiers temporaires, telles que "withTempDirectory" pour créer un répertoire temporaire et "withTempFile" pour créer un fichier temporaire spécifique au répertoire courant. Vous pouvez en savoir plus sur ces fonctions dans la documentation du module.

# Voir aussi

- [Documentation du module System.IO.Temp](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Guide pratique Haskell sur la manipulation de fichiers](https://serokell.io/blog/haskell-file-manipulation)
- [Tutoriel Haskell pour débutants](https://www.haskell.org/tutorial/)