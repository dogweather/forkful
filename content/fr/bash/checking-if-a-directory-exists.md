---
title:                "Vérifier si un répertoire existe"
html_title:           "Bash: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Déterminer si un dossier existe est une méthode de vérification préalable courante dans la programmation Bash. On fait ça pour éviter les erreurs lorsqu'on tente d'accéder à un dossier qui n'existe pas.

## Comment faire :
Voici un exemple simple de vérification d'existence d'un dossier en Bash.

```Bash
# définir le nom du dossier
dir="/chemin/dossier"

# vérifier si le dossier existe
if [ -d "$dir" ]; then
    echo "Le dossier existe"
else
    echo "Le dossier n'existe pas"
fi
```

Si le dossier existe, ce script Bash affichera "Le dossier existe". Sinon, il affichera "Le dossier n'existe pas".

## Plongée en profondeur
Historiquement, les programmes Bash vérifiaient l'existence d'un dossier pour éviter les erreurs de fichier non trouvé (`file not found`). Aujourd'hui, c'est une pratique courante pour améliorer la robustesse du code.

Autres alternatives :
- Vous pouvez également utiliser la commande `test` comme alternative : `if test -d "$dir"`
- Les versions modernes de Bash permettent `[[ -d "$dir" ]]` qui est à la fois plus puissant et plus sûr.

Côté mise en œuvre, `-d` est une option intégrée à Bash qui vérifie l'existence d'un dossier. Elle renvoie vrai (`true`) si le dossier existe et faux (`false`) dans le cas contraire.

## Voir aussi
Vous pouvez souhaiter vérifier d'autres aspects d'un fichier ou dossier. Pour plus d'informations:
- Pour vérifier si un fichier existe : [Checking file existence](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- Pour vérifier si un fichier est lisible, accessible en écriture, ou exécutable : [File conditionals](https://tldp.org/LDP/abs/html/fto.html).