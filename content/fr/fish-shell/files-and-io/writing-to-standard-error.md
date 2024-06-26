---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:08.025145-07:00
description: "Comment faire : Dans Fish Shell, vous pouvez \xE9crire dans stderr en\
  \ redirigeant votre sortie en utilisant `>&2`. Voici un exemple simple ."
lastmod: '2024-03-13T22:44:58.342123-06:00'
model: gpt-4-0125-preview
summary: "Dans Fish Shell, vous pouvez \xE9crire dans stderr en redirigeant votre\
  \ sortie en utilisant `>&2`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
Dans Fish Shell, vous pouvez écrire dans stderr en redirigeant votre sortie en utilisant `>&2`. Voici un exemple simple :

```fish
echo "Ceci est un message d'erreur" >&2
```

Cette commande se contente de faire écho d'un message sur stderr au lieu de stdout. Si vous deviez écrire un script qui affiche à la fois des messages réguliers et des messages d'erreur, vous pourriez faire quelque chose comme ceci :

```fish
echo "Démarrage du processus"
echo "Une erreur s'est produite" >&2
echo "Processus terminé"
```

Exemple de sortie si vous exécutez le script et redirigez stderr vers un fichier :

```
Démarrage du processus
Processus terminé
```

Le message d'erreur n'apparaîtrait pas dans la sortie standard mais serait trouvé dans le fichier vers lequel vous avez redirigé stderr.

Dans des scénarios nécessitant une gestion des erreurs ou un enregistrement plus sophistiqués, Fish ne dispose pas de bibliothèques intégrées conçues explicitement pour cela. Cependant, vous pouvez utiliser des outils externes ou écrire des fonctions pour aider. Par exemple, créer une fonction de journalisation simple pourrait ressembler à ceci :

```fish
function log_error
    echo $argv >&2
end

log_error "Ceci est un message d'erreur avancé"
```

Cette fonction `log_error` prendra n'importe quelle chaîne que vous lui donnez et l'écrira dans stderr. Utiliser des fonctions comme celle-ci peut aider à garder votre gestion des erreurs propres et cohérente à travers vos scripts.
