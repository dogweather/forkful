---
title:                "Travailler avec les fichiers csv"
html_title:           "Gleam: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

##Pourquoi

Si vous travaillez avec des données, il est très probable que vous ayez été confronté au format CSV. Ce format simple et facile à comprendre est couramment utilisé pour stocker des données tabulaires telles que des feuilles de calcul et des bases de données.

##Comment faire

La bibliothèque standard de Gleam offre des fonctionnalités robustes pour lire et écrire des fichiers CSV. Jetons un coup d'œil à un exemple simple de lecture d'un fichier CSV et d'affichage du résultat :

```Gleam
let result = File.read_text("example.csv")

case result {
  Ok(contents) -> List.for_each(contents, { row ->
    let columns = Csv.parse_row(row, ";")
    show(columns)
  })
  Err(error) -> println("Une erreur s'est produite : $(error)")
}
```

Le code ci-dessus utilise la fonction `File.read_text` pour lire le contenu du fichier CSV dans une chaîne de caractères. Ensuite, la fonction `Csv.parse_row` est utilisée pour convertir chaque ligne en une liste de colonnes. Enfin, la fonction `List.for_each` est utilisée pour parcourir chaque ligne et afficher les colonnes à l'écran.

Le résultat de ce code sera une liste contenant toutes les lignes du fichier CSV, avec les colonnes séparées par des points-virgules. Par exemple, si notre fichier CSV contient les données suivantes :

```
nom;prenom;age
Doe;John;35
Smith;Jane;27
```

Le résultat serait :

```
liste(
  liste("nom", "prenom", "age"),
  liste("Doe", "John", "35"),
  liste("Smith", "Jane", "27")
)
```

Notez que la fonction `Csv.parse_row` prend un deuxième argument optionnel, spécifiant le délimiteur à utiliser pour séparer les colonnes. Par défaut, elle utilise la virgule, mais vous pouvez le remplacer par le caractère de votre choix, comme dans cet exemple où nous utilisons le point-virgule comme délimiteur.

##Plongée en profondeur

En plus de la lecture et de l'écriture de fichiers CSV, la bibliothèque standard de Gleam offre également des fonctionnalités pour travailler avec des données CSV en mémoire. Par exemple, vous pouvez utiliser les fonctions `Csv.parse_file` et `Csv.serialize` pour facilement convertir des données CSV en structures de données telles que des tableaux et des tuples, et vice-versa.

Vous pouvez également utiliser les fonctions `Csv.read` et `Csv.write` pour lire et écrire des données CSV ligne par ligne, plutôt que de les traiter en une seule fois. Cela peut être utile pour les fichiers CSV volumineux qui peuvent entraîner une utilisation excessive de la mémoire si les données sont traitées en même temps.

Pour une liste complète des fonctionnalités disponibles pour travailler avec des CSV en Gleam, consultez la documentation de [la bibliothèque standard CSV de Gleam](https://gleam.run/modules/csv/).

##Voir aussi

- [Documentation de Gleam](https://gleam.run/)
- [Documentation de la bibliothèque standard de Gleam](https://gleam.run/modules/)
- [Exemples de projets utilisant Gleam](https://github.com/search?q=language%3Agleam)