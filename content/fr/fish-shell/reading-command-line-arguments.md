---
title:                "Lecture des arguments de la ligne de commande"
html_title:           "Fish Shell: Lecture des arguments de la ligne de commande"
simple_title:         "Lecture des arguments de la ligne de commande"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi
Salut les moules ! Vous vous demandez peut-être pourquoi il est important de savoir lire les arguments en ligne de commande dans Fish Shell ? Eh bien, cela peut sembler un peu technique, mais en réalité, c'est une compétence très utile pour tout programmeur ou administrateur système. Cela vous permettra de manipuler les commandes de manière plus efficace, de gagner du temps et de personnaliser votre expérience avec Fish Shell.

# Comment faire
Coder dans un shell peut sembler intimidant, mais ne vous inquiétez pas, nous avons des astuces pour vous faciliter la tâche ! La syntaxe pour lire les arguments en ligne de commande dans Fish Shell est la suivante :

```Fish Shell
switch (argument)
case "valeur1"
    # exécutez une action si l'argument est égal à "valeur1"
case "valeur2"
    # exécutez une action si l'argument est égal à "valeur2"
case '*'
    # exécutez une action par défaut
end
```

Voyons un exemple concret. Imaginons que vous ayez un script Fish Shell nommé "mon_script.fish" et que vous l'exécutiez en utilisant la commande :

```Fish Shell
./mon_script.fish -n Marine
```
Dans ce cas, l'argument "-n" sera stocké dans la variable "argument" et le nom "Marine" sera stocké dans la variable "valeur1". Ainsi, si nous utilisons le code suivant :

```Fish Shell
switch (argument)
case "-n"
    echo "Bonjour $valeur1 !"
case "*"
    echo "Utilisez l'option -n pour spécifier votre nom."
end
```

Le résultat sera "Bonjour Marine !". Mais si nous n'avions pas utilisé l'option "-n", le résultat aurait été "Utilisez l'option -n pour spécifier votre nom.".

# Profonde plongée
Maintenant que vous savez comment lire les arguments en ligne de commande dans Fish Shell, vous pouvez aller plus loin en explorant les options avancées telles que l'utilisation de regex dans les cas ou la manipulation des tableaux pour gérer plusieurs arguments à la fois. Vous pouvez également consulter la documentation officielle de Fish Shell pour plus d'informations et de conseils sur la manipulation des arguments en ligne de commande.

# Voir aussi
- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutoriel vidéo sur la manipulation des arguments en ligne de commande dans Fish Shell](https://www.youtube.com/watch?v=dQw4w9WgXcQ)
- [Exemples de scripts Fish Shell sur GitHub](https://github.com/fish-shell/fish-shell/wiki/Scripting-examples)