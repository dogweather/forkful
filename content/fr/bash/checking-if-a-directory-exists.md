---
title:    "Bash: Vérifier si un répertoire existe"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Bash, il est important de savoir comment vérifier si un répertoire existe avant d'effectuer une tâche ou une commande. Cela peut vous faire économiser du temps et des efforts en évitant les erreurs dues à l'absence d'un répertoire requis.

## Comment faire

Pour vérifier si un répertoire existe en utilisant Bash, vous pouvez utiliser la commande `test` avec l'option `-d` pour spécifier que vous recherchez un répertoire. Par exemple:

```Bash
if test -d "/chemin/vers/mon/repertoire"; then
echo "Le répertoire existe!"
else
echo "Le répertoire n'existe pas."
fi
```

Cette commande vérifiera si le répertoire spécifié existe. Si c'est le cas, vous verrez l'annonce "Le répertoire existe!". Sinon, vous verrez "Le répertoire n'existe pas."

Vous pouvez également utiliser la commande `[[ ... ]]` pour la même tâche, en utilisant l'opérateur `-d` pour vérifier si un répertoire existe. Voici un exemple:

```Bash
if [[ -d "/chemin/vers/mon/repertoire" ]]; then
echo "Le répertoire existe!"
else
echo "Le répertoire n'existe pas."
fi
```

Vous pouvez également personnaliser votre message en utilisant la commande `printf`. Par exemple:

```Bash
if test -d "/chemin/vers/mon/repertoire"; then
printf "Le répertoire %s existe!\n" "/chemin/vers/mon/repertoire"
else
printf "Le répertoire %s n'existe pas.\n" "/chemin/vers/mon/repertoire"
fi
```

## Plongée en profondeur

Saviez-vous qu'il y a plusieurs autres options que vous pouvez utiliser avec la commande `test` pour vérifier si un répertoire existe? En plus de `-d`, vous pouvez également utiliser les options `-e` pour vérifier si un fichier ou un lien symbolique existe, `-f` pour vérifier si un fichier régulier existe, ou `-x` pour vérifier si un fichier est exécutable.

De plus, vous pouvez également vérifier si un répertoire existe en utilisant la commande `ls`. Par exemple:

```Bash
if ls "/chemin/vers/mon/repertoire" >/dev/null 2>&1; then
echo "Le répertoire existe!"
else
echo "Le répertoire n'existe pas."
fi
```

Cette commande vérifie si la liste du contenu du répertoire spécifié réussit ou échoue. Si elle réussit, cela signifie que le répertoire existe. Sinon, cela signifie qu'il n'existe pas.

## Voir aussi

- [Bash scripting for beginners](https://www.linux.com/training-tutorials/writing-simple-bash-script/)
- [Le guide avancé de Bash](https://tldp.org/LDP/abs/html/)
- [Guide de référence de Bash](https://www.gnu.org/software/bash/manual/html_node/index.html)