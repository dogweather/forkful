---
title:    "Gleam: Vérifier si un répertoire existe"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi
Saviez-vous qu'il existe une fonctionnalité très utile en utilisant le langage de programmation fonctionnelle, Gleam ? En effet, Gleam nous permet de vérifier si un répertoire existe avant d'effectuer une action. Mais pour quelle raison voudrait-on effectuer cette vérification ? C'est ce que nous allons explorer dans cet article.

## Comment faire 
Il existe plusieurs façons de vérifier l'existence d'un répertoire en utilisant Gleam. Voici deux exemples de code suivis de la sortie attendue :

```Gleam
exists_directory("mon_dossier/")
```

> ``true``

```Gleam
exists_directory("mon_autre_dossier")
```

> ``false``

Comme vous pouvez le constater, en utilisant la fonction `exists_directory()`, nous obtenons un booléen en réponse. Cela nous permet de savoir si oui ou non, le répertoire que nous cherchons existe bien.

## Plongée Profonde
Vous vous demandez peut-être pourquoi nous aurions besoin de vérifier si un répertoire existe avant d'exécuter une action. Eh bien, cela peut être très utile si nous voulons éviter des erreurs dans notre code. Par exemple, si nous voulons créer un nouveau fichier dans un répertoire spécifique, mais que celui-ci n'existe pas encore, nous pouvons utiliser la fonction `exists_directory()` pour vérifier et créer le répertoire si nécessaire avant de continuer avec notre action.

## Voir Aussi
Si vous souhaitez en savoir plus sur les fonctionnalités de Gleam, vous pouvez consulter les liens suivants :

- [Documentation officielle de Gleam](https://gleam.run/)
- [Tutoriels Gleam](https://www.youtube.com/playlist?list=PLJXLvumqr7IEBL5x4xdy7Xi_ms1ci-e1m)

Maintenant que vous connaissez la fonction `exists_directory()` en Gleam, à vous de jouer ! N'hésitez pas à l'expérimenter dans votre prochain projet.