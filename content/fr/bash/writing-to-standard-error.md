---
title:                "Écrire vers l'erreur standard"
html_title:           "Bash: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Écrire dans la sortie d'erreur standard peut sembler étrange, mais c'est en fait une pratique courante chez les programmeurs. Cela permet d'afficher des messages d'erreur ou de débogage lors de l'exécution d'un script, afin d'aider à identifier et résoudre les problèmes plus rapidement.

## How to:

Voici deux exemples de la façon d'écrire dans la sortie d'erreur standard en utilisant Bash:

```
#!/bin/bash

# Exemple 1: Afficher un message d'erreur
echo "Une erreur s'est produite!" >&2

# Exemple 2: Afficher un message de débogage
echo "Variable x = $x" >&2
```

Lorsque ces commandes sont exécutées, les messages seront affichés dans la sortie d'erreur plutôt que dans la sortie standard. Voici à quoi cela ressemblerait une fois le script terminé:

```
$ bash script.sh
Une erreur s'est produite!
Variable x = 10
```

## Deep Dive:

### Contexte historique:

La pratique d'écrire dans la sortie d'erreur standard provient des premiers systèmes UNIX, où les messages d'erreur étaient souvent envoyés à un fichier de journal. Cependant, cela posait des problèmes de sécurité car n'importe quel utilisateur pouvait accéder à ce fichier. Afin de résoudre ce problème, le concept de sortie d'erreur standard a été introduit pour séparer les messages d'erreur des messages normaux.

### Alternatives:

Bien qu'écrire dans la sortie d'erreur standard soit une pratique courante, il existe d'autres façons d'afficher des messages d'erreur ou de débogage. Par exemple, certains programmeurs utilisent des commandes tels que `logger` ou `syslog` pour envoyer des messages à des fichiers de journal dédiés systèmes. Cependant, cela peut être plus complexe et nécessite souvent des autorisations spéciales.

### Détails de mise en œuvre:

La syntaxe utilisée pour écrire dans la sortie d'erreur standard est `>&2`. Cela signifie que le curseur d'écriture est redirigé vers le descripteur de fichier 2, qui correspond à la sortie d'erreur standard. De plus, il est important de noter que la sortie d'erreur standard est également associée au descripteur de fichier 3, ce qui signifie que vous pouvez utiliser `>&3` pour envoyer des messages d'erreur à celui-ci si nécessaire.

## See Also:

Pour en savoir plus sur l'écriture dans la sortie d'erreur standard, consultez ces sources utiles :

- [The Linux Command Line - Standard I/O and Redirection](https://linuxcommand.org/lc3_lts0070.php)
- [GNU Bash Manual - Redirections](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)