---
title:                "Javascript: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte en Javascript ?

Écrire un fichier texte en Javascript peut sembler assez simple, mais cela peut être très utile dans de nombreux cas. Grâce à cette méthode, vous pouvez stocker et manipuler des données dans un fichier, qui peuvent être lues et utilisées à votre guise. Cela peut être particulièrement utile pour stocker des configurations, des données de jeu, des listes de tâches, etc.

# Comment faire pour écrire un fichier texte en Javascript ?

Pour écrire un fichier texte en Javascript, vous aurez besoin d'utiliser le module "fs". Ce module est disponible dans les versions les plus récentes de Node.js et vous permettra de travailler avec des fichiers en utilisant différentes méthodes.

Tout d'abord, vous devrez créer un objet de type "fs", comme ceci :

```Javascript
const fs = require('fs');
```

Ensuite, vous pouvez utiliser la méthode "writeFile()" pour écrire du contenu dans un fichier spécifique. Voici un exemple de code qui écrira une chaîne de caractères dans un fichier nommé "texte.txt" :

```Javascript
fs.writeFile('texte.txt', 'Ceci est un exemple de texte en Javascript.', function (err) {
  if (err) throw err;
  console.log('Le fichier a été écrit avec succès !');
});
```

Si vous ouvrez maintenant le fichier "texte.txt", vous verrez que la chaîne de caractères a été écrite dedans.

# Plongée en profondeur

Maintenant, vous vous demandez peut-être : "Et si je veux juste ajouter du contenu à un fichier existant ?" Ne vous inquiétez pas, il existe une méthode pour cela aussi. La méthode "appendFile()" vous permet d'ajouter du contenu à la fin d'un fichier existant, sans écraser son contenu précédent.

Voici un exemple de code qui utilise cette méthode :

```Javascript
fs.appendFile('texte.txt', '\nAjout d\'une nouvelle ligne de texte.', function (err) {
  if (err) throw err;
  console.log('Le contenu a été ajouté avec succès !');
});
```

Vous remarquerez que nous avons ajouté un "\n" avant la nouvelle ligne de texte. Cela crée une ligne vide entre le contenu précédent et le nouveau contenu ajouté.

# Voir aussi

- Documentation officielle de Node.js sur le module "fs" : https://nodejs.org/api/fs.html#fs_file_system
- Tutoriel vidéo sur l'écriture et la lecture de fichiers en Javascript : https://www.youtube.com/watch?v=e2se4th1wyw