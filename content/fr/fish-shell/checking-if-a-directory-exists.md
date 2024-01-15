---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Fish Shell: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous utilisez le Fish Shell, vous pourriez à un moment donné avoir besoin de vérifier si un répertoire existe avant de poursuivre votre code. Cet article va vous montrer comment le faire facilement. 

## Comment faire

```Fish Shell
if test -d /chemin/vers/mon/repertoire
	echo "Le répertoire existe"
end
```
Supposons que notre répertoire existe. Le code ci-dessus va afficher "Le répertoire existe" dans le terminal. Si le répertoire n'existe pas, rien ne sera affiché.

## Plongée en profondeur

La commande `test` permet de tester différentes conditions en Fish Shell. Ici, nous utilisons l'option `-d` qui vérifie si le chemin spécifié est un répertoire. Si c'est le cas, elle renvoie `true` et le code à l'intérieur du bloc `if` est exécuté. Sinon, elle renvoie `false` et le code à l'intérieur est ignoré.

Vous pouvez également utiliser la commande `test` avec d'autres options pour réaliser d'autres types de tests, tels que la vérification de l'existence d'un fichier ou d'un lien symbolique.

# Voir aussi

- La documentation officielle de Fish Shell sur la commande `test`: https://fishshell.com/docs/current/cmds/test.html
- Un article sur la syntaxe des conditions en Fish Shell: https://medium.com/@utkarsh_singh/condition-testing-in-fish-shell-511b9d025624