---
date: 2024-01-27 16:20:47.160074-07:00
description: "Modifier des fichiers sur place avec des lignes de commande (CLI - Command\
  \ Line Interface) en Ruby vous permet de modifier des fichiers directement depuis\u2026"
lastmod: '2024-03-13T22:44:58.415490-06:00'
model: gpt-4-0125-preview
summary: "Modifier des fichiers sur place avec des lignes de commande (CLI - Command\
  \ Line Interface) en Ruby vous permet de modifier des fichiers directement depuis\
  \ votre terminal, sans avoir besoin de les ouvrir dans un \xE9diteur, d'apporter\
  \ des modifications et de les sauvegarder."
title: "Modification de fichiers sur place avec des lignes de commande en une seule\
  \ \xE9tape"
weight: 32
---

## Quoi & Pourquoi ?

Modifier des fichiers sur place avec des lignes de commande (CLI - Command Line Interface) en Ruby vous permet de modifier des fichiers directement depuis votre terminal, sans avoir besoin de les ouvrir dans un éditeur, d'apporter des modifications et de les sauvegarder. Cette technique est incroyablement utile pour des modifications rapides, des mises à jour en lot, ou automatiser des tâches répétitives, économisant ainsi du temps et de l'effort.

## Comment faire :

Ruby offre une manière simple de modifier des fichiers sur place directement depuis la ligne de commande. En utilisant l'option `-i` de Ruby, vous pouvez indiquer à Ruby de travailler directement sur le(s) fichier(s) fourni(s). Jouons avec quelques exemples pour voir comment cela fonctionne dans la pratique. Imaginez que vous ayez un fichier `greetings.txt` avec le contenu suivant :

```
Bonjour, le monde !
Bonjour, Ruby !
Bonjour, programmation !
```

Et vous voulez remplacer le mot "Bonjour" par "Salut". Voici comment vous pouvez le faire :

```Ruby
ruby -i -pe "gsub(/Bonjour/, 'Salut')" greetings.txt
```

Après avoir exécuté cette commande, `greetings.txt` sera mis à jour pour :

```
Salut, le monde !
Salut, Ruby !
Salut, programmation !
```

Si vous êtes inquiet à l'idée de potentiellement gâcher des données, Ruby vous couvre. En fournissant une extension à l'option `-i`, Ruby crée une sauvegarde avant d'exécuter les changements. Par exemple :

```Ruby
ruby -i.bak -pe "gsub(/Bonjour/, 'Adieu')" greetings.txt
```

Maintenant, en plus de votre `greetings.txt` édité, vous trouverez un `greetings.txt.bak` dans le même répertoire, contenant le contenu original.

## Plongée Profonde

La magie de l'édition de fichiers sur place en Ruby provient de sa combinaison des capacités de traitement de texte à la Perl et de l'élégance syntaxique propre à Ruby. Historiquement, Perl était le langage de prédilection pour les scripts rapides en une ligne, surtout pour la manipulation de texte. Ruby a adopté ce paradigme, permettant des capacités puissantes de script en ligne de commande.

Il existe des alternatives pour l'édition sur place dans d'autres langages, comme Perl lui-même et sed, un éditeur de flux dans les systèmes Unix. Chacun a ses points forts — Perl est connu pour sa capacité de traitement de texte tandis que sed est inégalé dans sa simplicité pour les tâches d'édition de flux. Cependant, Ruby offre un équilibre, fournissant une manipulation de texte robuste avec une syntaxe plus lisible et conviviale, surtout pour ceux déjà familiers avec Ruby.

Sur le front de l'implémentation, l'édition sur place de Ruby fonctionne en renommant le fichier original, en créant un nouveau avec le nom de fichier original, puis en écrivant les changements dans ce nouveau fichier au fur et à mesure qu'il lit l'original renommé. Cette approche assure l'atomicité de l'opération ; soit le fichier entier est traité avec succès, soit aucun changement n'est fait, protégeant l'intégrité de vos données pendant le processus d'édition. Ce mécanisme, combiné avec la gestion des exceptions de Ruby, offre également une résilience contre les interruptions, telles que les pannes de courant ou les arrêts de processus, assurant que la sauvegarde reste intacte.

En résumé, l'édition de fichiers sur place de Ruby est un témoignage de son utilité en tant que langage de script, offrant un mélange de puissance, simplicité et élégance pour les tâches de manipulation de texte directement depuis la ligne de commande.
