---
title:    "Bash: Vérification de l'existence d'un dossier"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou même expérimenté, il peut être utile de savoir si un répertoire existe avant de l'utiliser dans votre code. Cela peut vous éviter des erreurs et des bugs ennuyeux. Dans cet article, nous allons explorer différentes façons de vérifier si un répertoire existe dans un script Bash.

## Comment faire

Il existe plusieurs méthodes pour vérifier l'existence d'un répertoire en Bash, mais nous allons en couvrir trois dans cet article. Tout d'abord, vous pouvez utiliser la commande `if [-d [répertoire]]` pour voir si un répertoire existe ou non. Par exemple:

```Bash
if [ -d "/chemin/vers/repertoire" ]; then
    echo "Le répertoire existe!"
else
    echo "Le répertoire n'existe pas!"
fi
```

Nous utilisons ici la condition `if [-d [répertoire]]` pour vérifier si le répertoire existe. Si c'est le cas, nous affichons un message indiquant que le répertoire existe. Sinon, nous affichons un autre message indiquant que le répertoire n'existe pas. Vous pouvez également utiliser la commande `test` pour la même vérification:

```Bash
if test -d "/chemin/vers/repertoire"; then
    echo "Le répertoire existe!"
else
    echo "Le répertoire n'existe pas!"
fi
```

La deuxième méthode consiste à utiliser l'opérateur de négation `!` devant la commande `test` ou `[-d [répertoire]]` pour vérifier si le répertoire n'existe PAS. Par exemple:

```Bash
if ! test -d "/chemin/vers/repertoire"; then
    echo "Le répertoire n'existe pas!"
else
    echo "Le répertoire existe!"
fi
```

Enfin, vous pouvez utiliser la commande `[[ ... ]]` qui est principalement utilisée dans les scripts Bash modernes pour la vérification de fichiers et de répertoires. Par exemple:

```Bash
if [[ -d "/chemin/vers/repertoire" ]]; then
    echo "Le répertoire existe!"
else
    echo "Le répertoire n'existe pas!"
fi
```

Celles-ci sont les méthodes les plus couramment utilisées pour vérifier si un répertoire existe en Bash. Vous pouvez choisir celle qui vous convient le mieux en fonction de votre code et de vos préférences personnelles.

## Profondeur

Dans cette section, nous allons plonger un peu plus profondément dans la vérification de l'existence d'un répertoire en Bash. La commande `test` utilisée pour vérifier l'existence d'un répertoire peut être utilisée pour d'autres opérations telles que la vérification de l'existence d'un fichier, la vérification de l'écriture ou de la lecture d'un fichier, etc. Vous pouvez également utiliser l'option `-f` pour vérifier si le chemin donné est un fichier ordinaire, `-e` pour vérifier si le chemin existe, `-r` pour vérifier la lecture, `-w` pour vérifier l'écriture et ainsi de suite.

Vous pouvez également utiliser la commande `mkdir` pour créer un répertoire s'il n'existe pas déjà. Si vous souhaitez créer des répertoires imbriqués, vous pouvez utiliser l'option `-p`. Voici un exemple:

```Bash
mkdir -p /chemin/vers/repertoires/imbriques
```

Cela créera le répertoire `chemin/vers/repertoires/imbriques` s'il n'existe pas déjà, ainsi que tous les répertoires parents nécessaires. Cela peut être utile si vous devez créer un répertoire pour stocker des fichiers ou des données dans votre script.

## Voir aussi

Pour plus d'informations sur la vérification de l'existence de répertoires en Bash, vous pouvez consulter les ressources suivantes:

- [Utilisation de la commande if en Bash](https://www.linode.com/docs/guides/using-the-if-command-in-bash/)
- [Explication de la commande test en Bash](https://www.golinuxcloud.com/bee/linux-tutorial/test-expressions-operators-in-linux-shell-script-bash/)
- [Création de répertoires en Bash avec mkdir](https://www.tutorialspoint.com/un