---
title:                "Travailler avec les fichiers csv"
html_title:           "Javascript: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est ? 
CSV (Comma-Separated Values) est un format de fichier couramment utilisé pour stocker des données sous forme de tableaux en utilisant des virgules comme séparateur. Les développeurs utilisent souvent des fichiers CSV pour stocker des données tabulaires telles que les données de vente, les stocks ou les listes d'étudiants.

# Pourquoi les programmeurs l'utilisent-ils ? 
Les fichiers CSV sont faciles à lire et à écrire, compatibles avec une grande variété de programmes et peuvent être importés dans des applications telles que des tableurs pour une analyse plus approfondie. Cela rend les fichiers CSV très utiles pour le stockage et l'échange de données entre différentes applications.

# Comment faire : 
Voici un exemple simple de code JavaScript pour lire un fichier CSV et afficher son contenu :

```javascript
const url = "monfichier.csv";
fetch(url)
  .then(response => response.text())
  .then(data => console.log(data))
```

Et voici comment écrire des données dans un fichier CSV :

```javascript
const data = [
   ["Nom", "Prénom", "Âge"],
   ["Dupont", "Jean", 25],
   ["Martin", "Julie", 30]
];
const csvContent = "data:text/csv;charset=utf-8," + data.map(row => row.join(",")).join("\n");
const encodedUri = encodeURI(csvContent);
window.open(encodedUri);
```

# Plongée en profondeur :
Les fichiers CSV ont été développés dans les années 1970 comme un moyen de stocker des données dans des tableaux en utilisant des ordinateurs obsolètes. Aujourd'hui, les fichiers CSV sont encore largement utilisés en raison de leur simplicité et de leur compatibilité.

Bien qu'ils soient pratiques pour stocker des données tabulaires, les fichiers CSV peuvent être limités en termes de capacités d'organisation et de manipulation des données. Certaines alternatives populaires incluent les fichiers JSON et les bases de données relationnelles.

En JavaScript, il existe plusieurs bibliothèques telles que PapaParse et CSV.js qui facilitent le travail avec les fichiers CSV en offrant des méthodes pour lire, écrire et manipuler les données.

# À voir aussi :
- [PapaParse](https://www.papaparse.com/) - Une bibliothèque JavaScript open-source pour analyser les fichiers CSV.
- [CSV.js](https://github.com/knrz/CSV.js) - Une autre bibliothèque JavaScript pour travailler avec les fichiers CSV.
- [JSON vs CSV: Which one to Choose?](https://medium.com/swlh/json-vs-csv-which-one-to-choose-a52d54db0637) - Un article comparant les fichiers JSON et CSV.