---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Écrire sur l'erreur standard permet d'envoyer des messages d'erreur séparément des résultats normaux. Les programmeurs le font pour faciliter le débogage et la gestion des erreurs.

## Comment faire :

Voici comment rediriger la sortie d'erreur standard (`stderr`) dans Fish Shell.

```Fish Shell
# Envoyer un message d'erreur simple vers stderr
echo "Erreur: quelque chose s'est mal passé" >&2

# Exemple avec une commande qui échoue et la redirection de l'erreur
false 2> erreur.log
cat erreur.log
```

Sortie attendue pour `cat erreur.log` (si la commande précédente a échoué) :
```
false: command not found
```

## Plongée en profondeur

Avant, les scripts étaient limités à une seule sortie, ce qui compliquait la distinction entre les résultats et les erreurs. `stderr` fut créé pour cette séparation. D'autres shells comme Bash ou Zsh utilisent '2>', mais Fish utilise `>&2` pour simplifier la syntaxe. En interne, `stderr` est un canal avec le descripteur de fichier 2.

## Voir aussi

- [Fish Documentation on Redirection](https://fishshell.com/docs/current/tutorial.html#redirection)
- [POSIX Standard on Standard Streams](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_206)