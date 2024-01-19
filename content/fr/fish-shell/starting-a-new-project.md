---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

Commencer un nouveau projet en programmation, c'est comme écrire une histoire à partir de rien. Les programmeurs le font pour résoudre un problème, pour créer un service, ou simplement pour apprendre et se perfectionner dans un nouveau langage de programmation.

## Comment faire :

Voici comment créer un nouveau script Fish Shell :

```Fish Shell
touch my_script.fish
open my_script.fish
```

Ensuite, écrivez votre script :

```Fish Shell
function hello
    echo "Bonjour, le monde!"
end
```

Maintenant, c'est le moment de l'exécuter :

```Fish Shell
. my_script.fish
hello
```

Votre terminal donnera l'impression suivante :

```Fish Shell
Bonjour, le monde!
```

## Plongée plus profonde

Le Fish Shell, introduit en 2005, s'est démarqué notamment par une interface conviviale, des suggestions automatiques, et une syntaxe plus compatible avec les scripts POSIX. Toutefois, ne négligez pas d'autres coquilles comme Bash ou Zsh. Chaque shell a ses forces et ses lacunes; à vous de choisir celui qui correspond le mieux à vos besoins. Lors de la création d'un nouveau projet, prenez en considération l'architecture, le déploiement, et la gestion de votre code source.

## Voir aussi

Pour une plongée encore plus profonde dans les entrailles du Fish Shell, jetez un coup d'œil aux liens suivants :

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Introduction à la programmation Shell sur Linux](https://openclassrooms.com/fr/courses/43538-reprenez-le-controle-a-laide-de-linux/40401-ecrire-un-script-shell)
- [Git & GitHub pour la gestion de code source](https://git-scm.com)

Notez que l'apprentissage constant est la clé de la maîtrise en programmation. Bonne chance !