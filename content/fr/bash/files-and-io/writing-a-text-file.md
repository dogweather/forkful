---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:01.337140-07:00
description: "Comment faire : Bash fournit des m\xE9thodes simples pour \xE9crire\
  \ dans un fichier. Les plus courantes sont l\u2019utilisation des op\xE9rateurs\
  \ de redirection (`>`,\u2026"
lastmod: '2024-03-13T22:44:58.017065-06:00'
model: gpt-4-0125-preview
summary: "Bash fournit des m\xE9thodes simples pour \xE9crire dans un fichier."
title: "R\xE9diger un fichier texte"
weight: 24
---

## Comment faire :
Bash fournit des méthodes simples pour écrire dans un fichier. Les plus courantes sont l’utilisation des opérateurs de redirection (`>`, `>>`) et la commande `tee`. Voici un rapide coup d’œil sur ces deux techniques.

En utilisant la redirection, vous pouvez écrire la sortie directement dans un fichier. L'opérateur `>` écrit le contenu dans un fichier, le remplaçant s'il existe déjà, tandis que `>>` ajoute au contenu d'un fichier existant sans supprimer son contenu. 

```bash
# Écrire dans un fichier avec >
echo "Bonjour, le monde !" > myfile.txt

# Ajouter à un fichier avec >>
echo "Ceci est une nouvelle ligne." >> myfile.txt
```

Si vous vérifiez le contenu de `myfile.txt` après avoir exécuté les commandes ci-dessus, vous trouverez :

```
Bonjour, le monde !
Ceci est une nouvelle ligne.
```

La commande `tee` est pratique quand vous voulez écrire dans un fichier et voir la sortie sur l'écran (stdout) simultanément. Par défaut, `tee` écrase le fichier, mais avec l'option `-a`, il ajoute au fichier.

```bash
# Écrire et afficher en utilisant tee
echo "Bonjour, à nouveau !" | tee myfile.txt

# Ajouter et afficher en utilisant tee -a
echo "Ajout d'une autre ligne." | tee -a myfile.txt
```

Après l'exécution de ces commandes, `myfile.txt` affichera :

```
Bonjour, à nouveau !
Ajout d'une autre ligne.
```

Bien que Bash lui-même offre de robustes capacités de manipulation de fichiers à travers la redirection et des commandes comme `tee`, des manipulations plus poussées ou des scénarios plus complexes pourraient nécessiter l'appel à des outils externes ou des langages de script (par exemple, Awk, Sed, Python) qui offrent des fonctions de traitement de texte plus sophistiquées. Cependant, pour la plupart des tâches d'écriture de fichier simples, les méthodes mentionnées ci-dessus sont pleinement suffisantes et largement utilisées.
