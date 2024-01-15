---
title:                "Travailler avec les fichiers CSV"
html_title:           "Rust: Travailler avec les fichiers CSV"
simple_title:         "Travailler avec les fichiers CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données tabulaires, il est probable que vous ayez souvent affaire à des fichiers CSV (Comma-Separated Values). Ces fichiers sont largement utilisés pour stocker et échanger des données entre différentes applications. La bonne nouvelle est que Rust offre une excellente prise en charge des fichiers CSV, ce qui en fait un excellent choix pour travailler avec ce format de données.

## Comment faire

Pour commencer à travailler avec des fichiers CSV en Rust, vous aurez besoin de la bibliothèque `csv`. Vous pouvez l'ajouter à votre projet en ajoutant la dépendance suivante à votre fichier `Cargo.toml` :

```Rust
[dependencies]
csv = "1.1.3"
```

Une fois que vous avez ajouté la dépendance, vous pouvez utiliser la bibliothèque en important le module `csv` dans votre code :

```Rust
use csv;
```

Maintenant, vous êtes prêt à lire et à écrire des fichiers CSV en utilisant les différentes fonctionnalités de la bibliothèque. Voici un exemple simple de lecture d'un fichier CSV et d'affichage de son contenu :

```Rust
use std::fs::File;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("exemples/fichier.csv")?; // ouvrir le fichier
    let mut rdr = csv::Reader::from_reader(file); // créer un lecteur CSV

    for result in rdr.records() {
        // parcourir chaque ligne du fichier
        let record = result?; // récupérer la ligne en cours
        println!("{:?}", record); // afficher la ligne
    }

    Ok(()) // retourner Ok si tout s'est bien passé
}
```

Et voici le résultat de l'exécution de ce programme :

```Rust
"John","Doe",25
"Jane","Smith",30
"Bob","Johnson",40
```

Vous pouvez également écrire des fichiers CSV en utilisant la bibliothèque `csv`. Voici un exemple de création d'un nouveau fichier CSV à partir de zéro et d'écriture de quelques lignes :

```Rust
use std::fs::File;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = csv::Writer::from_path("nouveau_fichier.csv")?; // créer un writer pour notre nouveau fichier

    wtr.write_record(&["Prenom", "Nom"])?; // écrire la première ligne du fichier
    wtr.write_record(&["John", "Doe"])?; // écrire la deuxième ligne
    wtr.write_record(&["Jane", "Smith"])?; // écrire la troisième ligne

    wtr.flush()?; // vider le buffer et écrire toutes les données sur le disque

    Ok(()) // retourner Ok si tout s'est bien passé
}
```

Et voici le contenu de notre nouveau fichier CSV :

```Rust
Prenom,Nom
John,Doe
Jane,Smith
```

Vous pouvez également spécifier des options supplémentaires lors de la lecture ou de l'écriture de fichiers CSV, comme la délimitation des colonnes, l'utilisation de guillemets, etc. Pour plus d'informations sur toutes les fonctionnalités offertes par la bibliothèque `csv`, vous pouvez consulter la documentation officielle à l'adresse suivante : [https://docs.rs/csv/1.1.3/csv/](https://docs.rs/csv/1.1.3/csv/).

## Plongée en profondeur

Si vous souhaitez en savoir plus sur la façon dont la bibliothèque `csv` fonctionne en interne, vous pouvez consulter son code source, qui est disponible sur GitHub à [https://github.com/BurntSushi/rust-csv](https://github.com/BurntSushi/rust-csv). Vous pouvez également contribuer au développement de la bibliothèque en soumettant des problèmes ou en proposant des pull requests.

## Voir aussi

- Documentation officielle de la bibliothèque `csv` : [https://docs.rs/csv/1.1.3/csv/](https://docs.rs/csv/1.1.3/csv/)
- Repository GitHub de la bibliothèque `csv` : [https://github.com/BurntSushi/rust-csv](https://github.com/BurntSushi/rust-csv)