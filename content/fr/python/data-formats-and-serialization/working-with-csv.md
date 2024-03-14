---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:52.487284-07:00
description: "Travailler avec les CSV (Valeurs S\xE9par\xE9es par des Virgules) implique\
  \ la lecture et l'\xE9criture de donn\xE9es dans des fichiers CSV, un format courant\
  \ pour le\u2026"
lastmod: '2024-03-13T22:44:57.260567-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec les CSV (Valeurs S\xE9par\xE9es par des Virgules) implique\
  \ la lecture et l'\xE9criture de donn\xE9es dans des fichiers CSV, un format courant\
  \ pour le\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec les CSV (Valeurs Séparées par des Virgules) implique la lecture et l'écriture de données dans des fichiers CSV, un format courant pour le stockage des données tabulaires. Les programmeurs le font pour échanger et stocker facilement des données dans un format texte simple, largement pris en charge sur différentes plateformes et langages.

## Comment faire :
Python fournit le module intégré `csv` pour gérer les fichiers CSV, ce qui rend la lecture et l'écriture simples. Pour une manipulation de données plus robuste et complexe, la bibliothèque tierce `pandas` est très populaire.

### Utilisation du module `csv`

#### Lire un fichier CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*En supposant que `sample.csv` contienne :*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Résultat :*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Écrire dans un fichier CSV
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Crée ou réécrit `output.csv` avec :*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Utilisation de `pandas` pour les CSV
`pandas` est une bibliothèque puissante pour la manipulation de données qui simplifie le travail avec les fichiers CSV parmi d'autres formats de données.

#### Installer pandas
```shell
pip install pandas
```

#### Lire un fichier CSV avec pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Résultat :*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Écrire dans un fichier CSV avec pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Crée ou réécrit `output_pandas.csv` avec :*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
