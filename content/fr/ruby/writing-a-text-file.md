---
title:    "Ruby: Écriture d'un fichier texte"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles quelqu'un pourrait vouloir écrire un fichier texte en programmation Ruby. Cela peut être utile pour stocker des données, créer des fichiers de configuration, ou même pour générer des rapports.

## Comment faire

Pour écrire un fichier texte en Ruby, il faut d'abord ouvrir un flux de données en utilisant la méthode `File.open()`. Ensuite, on peut écrire du contenu dans le fichier en utilisant la méthode `puts` tout en prenant soin de spécifier le flux de données sur lequel on veut écrire. Enfin, il faut fermer le fichier en utilisant la méthode `close`.

Voici un exemple de code :

```Ruby
# Ouvre un fichier en mode écriture
fichier = File.open("mon_fichier.txt", "w")

# Ecrit un message dans le fichier
fichier.puts "Bonjour à tous ! Je suis en train d'écrire un fichier texte."

# Ferme le fichier
fichier.close
```

Et voici le contenu du fichier "mon_fichier.txt" après l'exécution du code ci-dessus :

```
Bonjour à tous ! Je suis en train d'écrire un fichier texte.
```

## Plongée en profondeur

Il y a plusieurs choses à prendre en compte lors de l'écriture d'un fichier texte en Ruby. Tout d'abord, il est important de spécifier le mode d'écriture du fichier, qui peut être "w" pour écriture, "a" pour ajout, ou "r+" pour lecture et écriture.

Il est également possible de spécifier un encodage pour le fichier en utilisant la méthode `File.open()` avec l'option `:encoding`. Cela peut être utile lorsque l'on travaille avec des caractères spéciaux.

Il est également possible de gérer les erreurs lors de l'écriture du fichier en utilisant des blocs `begin`, `rescue` et `ensure`. Cela permet de s'assurer que le fichier sera fermé même en cas d'erreur lors de l'écriture.

## Voir aussi

- La documentation officielle sur l'écriture de fichiers en Ruby : https://ruby-doc.org/core-2.7.2/File.html
- Un tutoriel sur l'écriture de fichiers en Ruby : https://www.learnrubyonline.org/en/lessons/13