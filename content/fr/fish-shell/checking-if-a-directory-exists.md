---
title:                "Vérifier si un répertoire existe"
html_title:           "Fish Shell: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Vérifier si un répertoire existe est une opération courante dans la programmation, cela signifie simplement de déterminer si un chemin spécifique est actuellement un répertoire dans le système de fichiers. Les programmeurs le font pour éviter les erreurs lors de l'ouverture de fichiers ou lors de l'écriture dans un répertoire qui n'existe pas.

## Comment faire :

Voici comment vous pouvez le faire en Fish Shell :

```fish
# Définition du répertoire
set dir "/chemin/vers/le/répertoire"

# Vérification de l'existence du répertoire
if test -d $dir
    echo "Le répertoire existe."
else
    echo "Le répertoire n'existe pas."
end
```

Exemple de sortie pour un répertoire existant :

```fish
Le répertoire existe.
```

Et pour un répertoire inexistant :

```fish
Le répertoire n'existe pas.
```

## Exploration approfondie 

La commande `test -d` a été utilisée dans les shell Unix depuis les années 1980 pour vérifier l'existence d'un répertoire. Il existait des alternatives, comme `if [ -d $dir ]` dans Bash, mais Fish est resté proche de la tradition Unix.

Fish a également une manière alternative de vérifier si le répertoire existe, en utilisant une condition en ligne, comme ceci :

```fish
set dir "/chemin/vers/le/répertoire"; and test -d $dir; and echo "Le répertoire existe."; or echo "Le répertoire n'existe pas."
```

Cependant, cette méthode n'est pas aussi lisible que la méthode `if-else`.

## Voir aussi

Pour plus d'informations sur la programmation avec Fish shell, consultez les liens suivants :

- Documentation officielle de Fish : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)

- Tutoriel Fish Shell sur GitHub : [https://github.com/jorgebucaran/fish-cookbook](https://github.com/jorgebucaran/fish-cookbook)

- Manuel en ligne de la commande test : [http://man7.org/linux/man-pages/man1/test.1.html](http://man7.org/linux/man-pages/man1/test.1.html)