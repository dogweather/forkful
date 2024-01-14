---
title:    "Bash: Écrire vers le flux d'erreur standard"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire à la sortie d'erreur (standard error) peut sembler être une tâche peu commune pour certains programmeurs Bash. Cependant, c'est une pratique importante à comprendre et à maîtriser car elle permet de gérer efficacement les erreurs et les débogages dans vos scripts Bash.

## Comment faire

Pour écrire à la sortie d'erreur dans un script Bash, vous devez utiliser le numéro de descripteur de fichier standard 2, qui est réservé à la sortie d'erreur. Il est important de noter que le numéro de descripteur de fichier standard 1 est réservé à la sortie standard, qu'on utilise souvent avec la fonction echo.

Voici un exemple de code pour écrire à la sortie d'erreur :

```Bash
#!/bin/bash
echo "Ce message s'affiche à la sortie standard."
echo "Ce message s'affiche à la sortie d'erreur." >&2
```

Dans cet exemple, nous utilisons la syntaxe >&2 pour rediriger la sortie du message vers la sortie d'erreur. De cette façon, le message ne s'affichera pas dans la sortie standard mais sera plutôt envoyé à la sortie d'erreur, qui sera consultée uniquement en cas d'erreur ou de débogage.

Voici un exemple de sortie que vous pourriez obtenir en exécutant ce script :

```Bash
$ ./mon_script.sh
Ce message s'affiche à la sortie standard.
Ce message s'affiche à la sortie d'erreur.
```

Comme vous pouvez le constater, le message destiné à la sortie standard s'affiche normalement, tandis que le message destiné à la sortie d'erreur s'affiche dans la même couleur que les erreurs système, facilitant ainsi son identification.

## Plongée en profondeur

Maintenant que vous savez comment écrire à la sortie d'erreur dans vos scripts Bash, il est important de comprendre pourquoi c'est une pratique importante à utiliser. Tout d'abord, cela permet de séparer clairement les messages de sortie standard et les messages d'erreur, ce qui facilite la lecture et le débogage des scripts.

En outre, cela offre une meilleure gestion des erreurs et des exceptions. En envoyant les erreurs à la sortie d'erreur, vous pouvez facilement les repérer et les traiter correctement dans votre script. Vous pouvez également utiliser des instructions conditionnelles pour agir en fonction de la présence d'erreurs dans votre code.

Il est également possible d'utiliser la commande exit avec un code d'erreur spécifique pour indiquer la fin d'un script en cas d'erreur. Cela est très utile lors de l'exécution de scripts en série, où l'échec d'un script peut être géré dans le suivant en fonction du code d'erreur renvoyé.

## Voir aussi

Pour en savoir plus sur l'écriture à la sortie d'erreur dans Bash, vous pouvez consulter ces ressources :

- [Documentation Bash sur la redirection des entrées/sorties](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections)
- [Article sur l'utilisation de l'erreur standard en Bash](https://www.linux.com/topic/beginning-bash-programming-how-use-embedded-commands/)

Maintenant vous êtes prêts à écrire à la sortie d'erreur dans vos scripts Bash et à améliorer votre gestion d'erreurs et de débogage. N'hésitez pas à pratiquer et à expérimenter avec cette fonctionnalité pour devenir un expert en Bash !