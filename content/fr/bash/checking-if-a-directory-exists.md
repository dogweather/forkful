---
title:                "Bash: Vérifier si un répertoire existe"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous qu'il est possible de vérifier si un répertoire existe dans votre script Bash ? Cela peut sembler être une tâche banale, mais en réalité, c'est une fonctionnalité très utile qui peut vous faire gagner du temps et éviter des erreurs dans votre code.

## Comment faire

Pour vérifier si un répertoire existe dans votre script Bash, vous pouvez utiliser la commande "test" avec l'option "-d" suivie du chemin du répertoire que vous souhaitez vérifier. Par exemple :

```Bash
if [ -d "/chemin/du/repertoire" ]; then
    echo "Le répertoire existe !"
else
    echo "Le répertoire n'existe pas."
fi
```

Dans cet exemple, nous utilisons la commande "test" pour vérifier si le répertoire "/chemin/du/repertoire" existe. Si c'est le cas, nous affichons un message informant de l'existence du répertoire. Sinon, nous affichons un message indiquant que le répertoire n'existe pas. 

## Plongée en profondeur

La commande "test" est un outil très pratique pour effectuer des tests dans un script Bash. Avec l'option "-d", elle permet de vérifier si un répertoire existe. Cependant, il est important de noter que la commande "test" utilise le répertoire de travail en cours pour vérifier l'existence du répertoire. Si vous souhaitez vérifier l'existence d'un répertoire à un emplacement spécifique, vous devez spécifier le chemin complet du répertoire.

De plus, il est possible d'utiliser la commande "test" avec d'autres options pour effectuer différentes vérifications sur un répertoire, comme par exemple :

- "-r" pour vérifier si un répertoire est lisible
- "-w" pour vérifier si un répertoire est inscriptible
- "-x" pour vérifier si un répertoire est exécutable

En utilisant une combinaison de ces options, vous pouvez effectuer des tests plus avancés sur un répertoire dans votre script.

## Voir aussi

- [Documentation de la commande "test" (en anglais)](https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html#test-invocation)
- [Différentes options de la commande "test" (en anglais)](http://tldp.org/LDP/abs/html/tests.html)