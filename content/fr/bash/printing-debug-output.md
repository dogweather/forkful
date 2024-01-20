---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Imprimer la sortie de débogage c'est écrire des infos de contrôle pendant l'exécution de votre programme. On le fait pour visualiser ce qui se passe "dans la tête" de notre script.

## Comment faire:
Repérer un bug est plus facile si on peut suivre le flux de données. Voici comment afficher un message debug en Bash:

```Bash
#!/bin/bash

for i in {1..5}
do
   echo "Le compteur est maintenant: $i" >&2
done
```
Exécutez ce script. Vous verrez apparaître chaque valeur du compteur comme message débogage dans la console.

## Plongée Profonde
Historiquement, Bash a toujours eu la faculté d'imprimer les sorties de débogage. Cela dit, il existe d'autres méthodes pour le débogage, par exemple, utiliser un IDE avec un débogueur intégré. Quant à l'implémentation, Bash redirige le flux de sortie standard vers stdout (le plus souvent votre console), mais vous pouvez aussi le rediriger vers un fichier.

## Voir Aussi 
Pour plus d'informations, consultez:
- [Redirections de flux en bash](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.fr.html): une introduction aux redirections de flux en bash.
- [Bash Scripting Cheatsheet](https://devhints.io/bash): une feuille de triche avec les commandes de script Bash les plus courantes.