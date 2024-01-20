---
title:                "Écrire vers l'erreur standard"
html_title:           "Fish Shell: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font?

Ecrire sur la sortie d'erreur standard (standard error) est une pratique courante dans la programmation. Il s'agit d'écrire des messages d'erreur ou de débogage directement dans la console plutôt que dans la sortie standard (standard output). Les programmeurs le font pour rendre leur code plus clair, notamment lorsqu'ils essaient de localiser et de résoudre des erreurs.

## Comment faire:

```
Fish Shell propose la commande "echo" pour écrire sur la sortie d'erreur standard. Voici un exemple de l'utiliser pour afficher un message d'erreur:

```Fish Shell
echo "Erreur : nom de fichier invalide"

``` 
Le résultat dans la console serait :

```
Erreur : nom de fichier invalide
```

## Plongée en profondeur:

Ecrire sur la sortie d'erreur standard a été rendu populaire par la pratique de la programmation en shell, qui utilise souvent la console comme principale interface utilisateur. Cependant, d'autres langages de programmation offrent également cette fonctionnalité, comme le C et le Java. 

Une alternative à l'écriture sur la sortie d'erreur standard est l'utilisation de journaux de débogage, qui stockent les messages dans un fichier pour une consultation ultérieure. Cependant, cela peut être moins pratique pour un débogage en temps réel.

Techniquement, écrire sur la sortie d'erreur standard est réalisé en dirigeant les messages vers le descripteur de fichier approprié (standard error est généralement le descripteur 2). Cette pratique est souvent combinée avec la redirection de la sortie standard vers un fichier à l'aide de l'opérateur ">>".

## Voir aussi:

- [Documentation officielle Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutoriel sur l'utilisation des erreurs standard en C](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)