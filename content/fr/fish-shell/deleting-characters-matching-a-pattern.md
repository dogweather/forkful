---
title:    "Fish Shell: Suppression de caractères correspondant à un motif"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi

Supprimer des caractères correspondant à un motif peut sembler être une opération simple, mais elle peut en fait être très utile dans certaines situations. Par exemple, cela peut être utile lorsque vous souhaitez nettoyer ou modifier une chaîne de caractères en supprimant des éléments indésirables.

# Comment faire

Pour supprimer des caractères correspondant à un motif dans Fish Shell, vous pouvez utiliser la fonction `string replace` suivie du motif à supprimer et de l'élément par lequel le motif sera remplacé. Voici un exemple de code :

```
Fish Shell > set phrase "Bonjour le monde!"
Fish Shell > echo $phrase
Bonjour le monde!
Fish Shell > string replace r"le \w+" $phrase ""
Bonjour !
```

Dans cet exemple, nous avons utilisé le motif "le" suivi d'un espace et d'un ou plusieurs caractères alphabétiques (`\w+`). Ce motif correspond à "le monde" dans la phrase initiale, que nous avons ensuite remplacé par une chaîne vide afin de le supprimer.

# Plongée en profondeur

Lorsque vous utilisez `string replace` pour supprimer des caractères correspondant à un motif, vous pouvez également utiliser des expressions régulières plus complexes. Par exemple, si vous souhaitez supprimer tous les nombres contenus dans une chaîne de caractères, vous pouvez utiliser le motif `\d+` qui correspond à un ou plusieurs chiffres. Voici un exemple de code :

```
Fish Shell > set phrase "Il y a 3 pommes et 5 oranges dans le panier."
Fish Shell > echo $phrase
Il y a 3 pommes et 5 oranges dans le panier.
Fish Shell > string replace r"\d+" $phrase ""
Il y a pommes et oranges dans le panier.
```

Vous pouvez également utiliser `string replace` pour supprimer des caractères à partir d'un fichier texte en utilisant la commande `grep`. Par exemple, si vous voulez supprimer toutes les lignes contenant le mot "erreur" dans un fichier de log, vous pouvez utiliser cette commande :

```
Fish Shell > cat log.txt
Ligne 1 - erreur système
Ligne 2 - erreur de connexion
Ligne 3 - tout va bien
Fish Shell > grep -v "erreur" log.txt > log_sans_erreurs.txt
Fish Shell > cat log_sans_erreurs.txt
Ligne 3 - tout va bien
```

# Voir aussi

- [Documentation officielle du Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutoriel sur les expressions régulières dans Fish Shell](https://dev.to/fulara/fish-shell-regular-expression-tutorial-1bha)
- [Liste des commandes Fish Shell](https://fishshell.com/docs/current/commands.html)