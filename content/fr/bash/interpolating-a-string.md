---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:50:18.452223-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Interpoler une chaîne, c'est insérer des valeurs de variables ou d'expressions directement dans le texte. C'est ultra-pratique pour intégrer des données dynamiques sans s'embrouiller entre concaténations et citations.

## How to (Comment faire) :
```Bash
#!/bin/bash
# Déclaration d'une variable
prenom="Alex"

# Interpolation simple avec $
echo "Bonjour, $prenom !"

# Avec des accolades pour plus de clarté
echo "Comment ça va, ${prenom} ?"

# Utilisation dans une commande
utilisateur=$(whoami)
echo "Vous êtes connecté en tant que $utilisateur."
```
Sortie :
```
Bonjour, Alex !
Comment ça va, Alex ?
Vous êtes connecté en tant que [votre nom d'utilisateur].
```

## Deep Dive (Plongée Profonde)
Historiquement, l'interpolation des chaînes vient des premiers jours de la programmation shell, facilitant la personnalisation des scripts en y intégrant des éléments dynamiques. En alternative, on peut concaténer avec `+`, mais en bash c'est lourd et moins lisible. Coté implémentation, Bash remplace simplement la variable par sa valeur pendant l'exécution, rien de compliqué.

## See Also (Voir Aussi)
- Bash manual: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash String Manipulation Examples: https://linuxconfig.org/bash-scripting-tutorial-for-beginners
