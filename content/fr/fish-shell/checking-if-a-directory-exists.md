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

# Qu'est-ce que c'est et pourquoi le faire ?

Vérifier si un répertoire existe est une étape importante pour les programmeurs car cela leur permet de s'assurer que leur code fonctionne correctement et qu'il peut accéder aux bons fichiers et répertoires nécessaires pour son exécution.

# Comment le faire ?

Voici un exemple de code en utilisant le Fish Shell pour vérifier si un répertoire existe :

```
if test -d ~/documents
  echo "Le répertoire 'documents' existe."
end
```

Output:

```
Le répertoire 'documents' existe.
```

# Plongée en profondeur

Historiquement, cette méthode de vérification de répertoire existe depuis longtemps dans les systèmes informatiques et elle est également utilisée dans d'autres shells que le Fish Shell, comme le Bash ou le zsh.

D'autres alternatives pour vérifier si un répertoire existe incluent l'utilisation de la commande `ls` avec l'option `-d` ou l'utilisation de la commande `find`.

La commande `test` utilisée dans notre exemple peut également être utilisée pour vérifier l'existence d'autres types de fichiers, tels que les fichiers réguliers ou les liens symboliques.

# Voir aussi

Pour en savoir plus sur la commande `test` et ses différentes options, vous pouvez consulter la documentation officielle : [https://fishshell.com/docs/current/cmds/test.html](https://fishshell.com/docs/current/cmds/test.html)

Si vous souhaitez en apprendre davantage sur le Fish Shell et ses fonctionnalités, vous pouvez consulter le site officiel : [https://fishshell.com/](https://fishshell.com/)