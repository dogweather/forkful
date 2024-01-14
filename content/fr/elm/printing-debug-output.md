---
title:                "Elm: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez en Elm, il peut être extrêmement utile de pouvoir afficher des informations de débogage pour comprendre le comportement de votre code. Cela vous permet de déterminer les erreurs et de mieux comprendre les valeurs de vos variables.

## Comment faire

Pour afficher des informations de débogage en Elm, il suffit d'utiliser la fonction `Debug.log` et de lui passer une chaîne de caractères ainsi que la valeur que vous souhaitez afficher. Par exemple :

```Elm
monMessage = Debug.log "Voici mon message de débogage !" "Valeur à afficher"
```
Cela affichera dans votre Console de débogage le message "Voici mon message de débogage ! : Valeur à afficher". Cela peut également être utile lorsque vous voulez suivre une variable tout au long de votre code et voir comment elle change.

## Plongée en profondeur

La fonction `Debug.log` n'est disponible que lorsque vous compilez en mode de développement. Cela signifie qu'elle n'affectera pas votre code en production et ne sera pas visible par les utilisateurs finaux.

Il est également important de noter que l'utilisation excessive de la fonction `Debug.log` peut ralentir votre code. Il est donc préférable de l'utiliser avec parcimonie et de la supprimer une fois le débogage terminé.

## Voir aussi

Si vous voulez en savoir plus sur l'utilisation de la fonction `Debug.log` en Elm, vous pouvez consulter les liens suivants :

- Documentation officielle sur la fonction Debug.log
- Article de blog sur les bonnes pratiques pour l'utilisation de la fonction Debug.log

N'oubliez pas que l'affichage de messages de débogage peut grandement faciliter votre processus de développement et vous faire gagner du temps en résolvant les erreurs plus rapidement. Alors n'hésitez pas à utiliser cette fonctionnalité utile en Elm !