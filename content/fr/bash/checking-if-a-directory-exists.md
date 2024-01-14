---
title:                "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est important de vérifier si un répertoire existe avant de continuer votre programme Bash. Eh bien, c'est une étape cruciale pour s'assurer que votre script fonctionne correctement et éviter les erreurs inattendues.

## Comment faire

La bonne nouvelle est qu'il est très facile de vérifier si un répertoire existe en utilisant quelques lignes de code Bash. Voici un exemple de code qui vérifie si un répertoire appelé "documents" existe dans votre répertoire Home :

```Bash
if [ -d ~/documents ]
then
  echo "Le répertoire existe."
else
  echo "Le répertoire n'existe pas."
fi
```

Dans cet exemple, nous utilisons la commande "[-d](https://wiki.bash-hackers.org/commands/classictest#the_determinator_command)" pour vérifier si le répertoire "documents" existe dans le répertoire Home, représenté par le symbole "~" en Bash. Si le répertoire existe, le programme affichera "Le répertoire existe." Sinon, il affichera "Le répertoire n'existe pas."

## Plongée en profondeur

Maintenant que vous savez comment vérifier si un répertoire existe en utilisant le code ci-dessus, vous pourriez vous demander quels autres outils et options vous pouvez utiliser pour ce processus. Voici quelques éléments à prendre en compte :

- Utilisez l'option "-e" pour vérifier si un fichier ou un répertoire existe, au lieu de la commande "[-d](https://wiki.bash-hackers.org/commands/classictest#the_determinator_command)" pour spécifier un fichier ou un répertoire spécifique.
- Vous pouvez également utiliser la commande "test" au lieu de "[-d](https://wiki.bash-hackers.org/commands/classictest#the_determinator_command)". Par exemple, "test -d ~/documents" fonctionnera de la même manière que "[-d](https://wiki.bash-hackers.org/commands/classictest#the_determinator_command) ~/documents".
- Pour vérifier si un répertoire existe dans un emplacement spécifique, vous pouvez utiliser le chemin absolu au lieu du chemin relatif.
- Vous pouvez également utiliser des variables pour spécifier le chemin du répertoire que vous souhaitez vérifier.

## Voir aussi

- [Documentation sur la commande '[-d'](https://wiki.bash-hackers.org/commands/classictest#the_determinator_command)
- [Documentation sur la commande 'test'](https://ss64.com/bash/test.html)
- [Article sur la vérification de la présence d'un répertoire en Bash](https://linuxhint.com/bash_check_if_a_directory_exists/)
- [Vidéo tutoriel sur la vérification de la présence d'un répertoire en Bash](https://www.youtube.com/watch?v=JaVcaRcyknI)

Maintenant que vous savez comment vérifier si un répertoire existe en Bash, vous pouvez utiliser cette étape dans vos scripts pour assurer une exécution sans avarie. N'hésitez pas à consulter les liens ci-dessus pour plus d'informations et de ressources utiles. Bon codage !