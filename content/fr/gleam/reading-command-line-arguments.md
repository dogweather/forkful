---
title:    "Gleam: Lecture des arguments de ligne de commande"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes développeur ou programmer en Gleam, il est essentiel de comprendre comment lire les arguments de ligne de commande. Cela vous permettra d'interagir avec votre programme et de le personnaliser en fonction des besoins de l'utilisateur.

# Comment faire

Pour lire les arguments de ligne de commande en Gleam, vous pouvez utiliser la fonction `os.args()` qui renvoie un tableau de chaînes de caractères contenant tous les arguments saisis lors de l'exécution du programme.

Voici un exemple de code montrant comment utiliser cette fonction :

```
Gleam

import os
import gleam/pretty-print

fn main() {
  let args = os.args()
  pretty-print.formatln("Les arguments saisis sont : {}", [args])
}
```

Si vous exécutez ce programme avec la commande `gleam run mon_programme.gleam arg1 arg2`, vous obtiendrez l'affichage suivant :

```
Les arguments saisis sont : ["arg1", "arg2"]
```

Vous pouvez ensuite utiliser ces arguments dans votre programme pour effectuer différentes actions en fonction des besoins de l'utilisateur.

# Plongée en profondeur

Il est important de noter que la fonction `os.args()` renvoie également le nom du programme en tant que premier argument. Par conséquent, si vous souhaitez uniquement obtenir les arguments saisis par l'utilisateur, vous devrez les extraire du tableau renvoyé par la fonction.

De plus, en utilisant le module `os`, vous pouvez également accéder à d'autres informations utiles concernant l'environnement et l'exécution de votre programme, telles que les variables d'environnement ou le répertoire de travail actuel.

# Voir aussi

- Documentation officielle sur les commandes de ligne de commande en Gleam : [lien](https://gleam.run/book/intro/Commandline_arguments.html)
- Tutoriel sur les arguments de ligne de commande en Gleam : [lien](https://dev.to/joelwurtz/comment-gerer-les-arguments-de-ligne-de-commande-en-gleam-1cje)
- Exemples de code pour différents cas d'utilisation des arguments de ligne de commande : [lien](https://github.com/search?q=language%3Agleam+command+line)