---
title:                "Analyse du code HTML"
html_title:           "Fish Shell: Analyse du code HTML"
simple_title:         "Analyse du code HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur web, vous avez probablement déjà dû faire face à la tâche fastidieuse de parcourir du code HTML pour extraire des informations précises. Heureusement, avec le shell Fish et quelques connaissances en parsing, vous pouvez automatiser cette tâche et libérer du temps pour des tâches plus importantes.

## Comment faire

Pour commencer, assurez-vous d'avoir Fish Shell installé sur votre système. Si ce n'est pas le cas, vous pouvez facilement l'installer avec votre gestionnaire de paquets préféré.

Une fois Fish Shell installé, vous pouvez commencer à écrire votre code de parsing HTML en utilisant la commande `read` pour lire le contenu d'un fichier ou `curl` pour récupérer le contenu d'une URL :

```
Fish Shell

# Récupérer le contenu d'une URL
set html (curl https://www.example.com)

# Lire le contenu d'un fichier
set html (read ~/Documents/example.html)
```

Ensuite, utilisez la commande `string split` pour diviser votre code HTML en parties plus petites, en utilisant une balise spécifique comme délimiteur. Par exemple, si vous voulez extraire toutes les informations situées entre les balises `<h1>` et `</h1>`, vous pouvez utiliser le code suivant :

```
Fish Shell

set html (curl https://www.example.com)
set headers (string split -rm "<h1>" "</h1>" $html)

# Affiche le contenu de chaque balise <h1>
for header in $headers
    echo $header
end
```

Enfin, utilisez la commande `string sub` pour retirer les balises inutiles et ne garder que le contenu souhaité :

```
Fish Shell

set html (curl https://www.example.com)
set headers (string split -rm "<h1>" "</h1>" $html)

# Affiche le contenu de chaque balise <h1> sans les balises
for header in $headers
    string sub -r "</?h1>" "" $header
end
```

## Plongée en profondeur

Il existe de nombreuses autres commandes utiles pour le parsing HTML dans Fish Shell, telles que `string match` pour trouver une sous-chaîne spécifique, `sed` pour effectuer des modifications plus complexes et `grep` pour filtrer les résultats. N'hésitez pas à explorer les possibilités avec Fish Shell et à lire la documentation officielle pour en apprendre davantage sur ces commandes.

## Voir aussi

- Documentation officielle de Fish Shell : https://fishshell.com/docs/current/index.html
- Tutoriel pour débuter avec Fish Shell : https://fishshell.com/docs/current/tutorial.html
- Guide du parsing HTML avec Fish Shell : https://fishshell.com/docs/current/cmds/string.html