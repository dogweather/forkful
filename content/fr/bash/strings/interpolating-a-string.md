---
date: 2024-01-20 17:50:18.452223-07:00
description: "How to (Comment faire) : Historiquement, l'interpolation des cha\xEE\
  nes vient des premiers jours de la programmation shell, facilitant la personnalisation\u2026"
lastmod: '2024-04-05T22:51:11.927689-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, l'interpolation des cha\xEEnes vient des premiers jours\
  \ de la programmation shell, facilitant la personnalisation des scripts en y int\xE9\
  grant des \xE9l\xE9ments dynamiques."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

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
