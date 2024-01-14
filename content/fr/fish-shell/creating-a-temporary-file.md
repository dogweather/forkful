---
title:                "Fish Shell: Création d'un fichier temporaire"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Pourquoi

Il y a de nombreuses raisons pour lesquelles vous pourriez avoir besoin de créer un fichier temporaire lors de la programmation en Fish Shell. Cela peut inclure stocker temporairement des données, écrire des fichiers provisoires pour des tests, ou même pour des raisons de sécurité en évitant de stocker des informations sensibles dans un fichier permanent. Dans tous les cas, la création de fichiers temporaires peut être une pratique utile et efficace pour les programmeurs.

##Comment faire

Il existe plusieurs façons de créer un fichier temporaire en utilisant Fish Shell. L'une des méthodes les plus simples consiste à utiliser la commande `mktemp`, qui génère un nom de fichier unique pour un fichier temporaire. Voici un exemple de code utilisant cette commande:

```Fish Shell
fichier_temp = (mktemp)
echo "Ceci est un fichier temporaire créé par Fish Shell" > $fichier_temp
echo "Contenu du fichier temporaire:"
cat $fichier_temp
```
Ce code génère un nom de fichier temporaire et y écrit le texte "Ceci est un fichier temporaire créé par Fish Shell". Ensuite, il affiche le contenu du fichier temporaire en utilisant la commande `cat`. Vous pouvez également modifier le texte de votre fichier temporaire en remplaçant la première commande `echo` par une chaine de caractères ou une variable contenant votre contenu.

Il est également possible d'utiliser d'autres commandes, telles que `touch` ou `cp`, pour créer et travailler avec des fichiers temporaires en Fish Shell. N'hésitez pas à explorer ces différentes options pour trouver celle qui convient le mieux à vos besoins.

##Deep Dive

Si vous souhaitez en savoir plus sur la création de fichiers temporaires en Fish Shell, il est important de comprendre les différentes options de la commande `mktemp`. Par exemple, vous pouvez spécifier un modèle pour le nom de fichier temporaire en utilisant l'option `-t`, ou même spécifier un répertoire de destination pour le fichier en utilisant l'option `-d`.

De plus, gardez à l'esprit que les fichiers temporaires sont automatiquement supprimés une fois que le processus en cours se termine. Cela peut être utile pour éviter l'encombrement de votre système avec des fichiers inutiles, mais cela peut également être un problème si vous avez besoin de conserver le fichier temporaire pour une raison quelconque. Dans ce cas, vous pouvez utiliser la commande `set -e` pour empêcher la suppression automatique du fichier temporaire.

##Voir aussi

Pour en savoir plus sur la création et l'utilisation de fichiers temporaires en Fish Shell, consultez les ressources suivantes:

- [Documentation Fish Shell](https://fishshell.com/docs/current/index.html)
- [Article sur la gestion des fichiers temporaires en Shell](https://www.linux.com/training-tutorials/how-manage-temporary-files-shell-scripting/)
- [Exemples de création de fichiers temporaires en Fish Shell](https://www.tutorialspoint.com/how-to-create-temporary-files-using-fish-shell)