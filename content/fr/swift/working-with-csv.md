---
title:                "Swift: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec des fichiers CSV en Swift?

Les fichiers CSV (Comma Separated Values) sont un format de données couramment utilisé pour stocker et échanger des données tabulaires. En travaillant avec des fichiers CSV en Swift, vous pouvez facilement importer ou exporter des données vers d'autres applications ou bases de données. Cela peut être particulièrement utile pour traiter de grandes quantités de données ou pour travailler avec des applications qui ne prennent en charge que les fichiers CSV.

## Comment faire?

Pour travailler avec des fichiers CSV en Swift, vous devez utiliser la bibliothèque "SwiftCSV", qui peut être facilement importée en utilisant Swift Package Manager. Une fois la bibliothèque ajoutée à votre projet, vous pouvez utiliser la classe CSV pour lire ou écrire des données CSV. Voici un exemple de code pour lire un fichier CSV et afficher son contenu:

```Swift
let fileURL = Bundle.main.url(forResource: "example", withExtension: "csv") // remplacer "example" par le nom de votre fichier CSV
do {
    let csv = try CSV(url: fileURL!)
    let rows = csv.namedRows // pour une version non nommée, utilisez csv.rows
    for row in rows {
        print(row) // imprime chaque ligne du fichier CSV
    }
} catch {
    print(error)
}
```

Lorsque vous exécutez ce code, vous devriez voir le contenu du fichier CSV imprimé dans la console. Vous pouvez également manipuler les données à votre guise en utilisant les méthodes fournies par la classe CSV, telles que la récupération d'une colonne spécifique ou l'ajout de nouvelles lignes.

## Plongée en profondeur

La bibliothèque SwiftCSV offre de nombreuses fonctionnalités pour faciliter la manipulation de fichiers CSV. En plus de lire et d'écrire des données, vous pouvez également effectuer des opérations de tri, de filtrage et de recherche sur les données CSV. De plus, la bibliothèque prend en charge la conversion des données en différents types, tels que les dates, les entiers ou les décimaux, pour une manipulation plus facile. Pour plus d'informations sur toutes les fonctionnalités disponibles, vous pouvez consulter la documentation de la bibliothèque.

## Voir aussi

- [SwiftCSV Documentation](https://github.com/naoty/SwiftCSV/wiki)
- [Swift Package Manager Documentation](https://swift.org/package-manager/)

Maintenant que vous avez appris comment travailler avec des fichiers CSV en Swift, vous pouvez commencer à intégrer cette fonctionnalité dans vos projets pour faciliter l'échange et le traitement de données. Bonne programmation!