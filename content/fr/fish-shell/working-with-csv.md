---
title:                "Travailler avec les fichiers csv"
html_title:           "Fish Shell: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi le faire? 
Travailler avec des fichiers CSV peut sembler intimidant pour les programmeurs débutants, mais c'est en fait un moyen pratique de stocker et d'organiser de grandes quantités de données, telles que des feuilles de calcul ou des bases de données. Cela peut être utile lors de la manipulation de données dans un programme ou lors de l'exportation de données vers d'autres applications.

Comment faire:
Pour lire un fichier CSV dans Fish Shell, utilisez la fonction `csv`. Par exemple, si vous avez un fichier appelé `donnees.csv`, voici comment vous pouvez afficher le contenu:

```fish
csv -d ";" foreach line in donnees.csv
echo $line
end
```
Ce code utilise la fonction `foreach` pour parcourir chaque ligne du fichier en utilisant le délimiteur `;`. Vous pouvez également spécifier un autre délimiteur en utilisant l'option `-d`. Pour en savoir plus sur les options disponibles, consultez la documentation de `csv` en utilisant la commande `man csv`.

Plongée en profondeur:
Les fichiers CSV sont couramment utilisés dans les applications de traitement de données, en particulier dans le domaine de la science des données. Ils ont été introduits dans les années 70 en tant que moyen de stocker des données sous forme de tableaux. Bien qu'ils soient pratiques, ils peuvent également présenter certains inconvénients, tels que la perte de précision lors de la manipulation de nombres à virgule flottante.

Voir aussi:
Si vous souhaitez en savoir plus sur les fichiers CSV et leur utilisation en programmation, consultez ces ressources:
- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Un tutoriel sur la manipulation de fichiers CSV en Python](https://realpython.com/python-csv/)
- [Une introduction aux fichiers CSV sur le site Khan Academy](https://fr.khanacademy.org/computing/hour-of-code/hour-of-code-tutorial/a/reading-and-writing-csv-files)