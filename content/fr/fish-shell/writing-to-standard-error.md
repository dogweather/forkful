---
title:    "Fish Shell: Écrire vers l'erreur standard"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Ecrire sur la sortie d'erreur standard peut sembler anecdotique, mais c'est en réalité une pratique importante dans la programmation. Cela permet de mieux gérer les erreurs et de déboguer plus efficacement votre code.

## Comment faire

La commande ```fish >&2```, lorsque utilisée dans votre script Fish Shell, enverra les sorties d'erreurs vers la sortie d'erreur standard, plutôt que vers la sortie standard. Cela signifie que les erreurs seront affichées à l'écran plutôt que d'être cachées dans un fichier de log.

Il est également possible de rediriger les erreurs vers un fichier spécifique, en utilisant la syntaxe suivante : ```fish command 2> error_log.txt```. Cela créera un fichier "error_log.txt" qui contiendra toutes les erreurs générées par la commande en question.

## Plongée en profondeur

La sortie d'erreur standard est également utile lors de la mise en place de scripts avec des pipelines, car cela permet de mieux gérer les erreurs à chaque étape. De plus, dans le cas où vous souhaitez enregistrer les erreurs dans un fichier tout en les affichant à l'écran, vous pouvez utiliser la syntaxe suivante : ```fish command > output.txt 2>&1```.

Cependant, il est important de noter que toutes les sorties d'erreur ne doivent pas forcément être redirigées vers la sortie d'erreur standard. Certains messages d'erreur peuvent être des avertissements plutôt que de véritables erreurs, et peuvent donc être redirigés vers la sortie standard en utilisant la syntaxe ```fish command 2>&1```.

## Voir aussi

- [La documentation officielle du Fish Shell](https://fishshell.com/docs/current/index.html)
- [Un guide complet sur l'utilisation des sorties d'erreur standard en programmation](https://www.tutorialspoint.com/unix/unix-io-redirections.htm)