---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "C++: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Le "parsing" d'une date à partir d'une chaîne de caractères est une fonctionnalité courante dans la programmation. Cela permet aux programmeurs de convertir une date sous la forme d'une chaîne de caractères en un objet de date reconnaissable par l'ordinateur. Cela est utile pour manipuler, comparer ou afficher des dates dans des applications.

## Comment Faire:

Voici un exemple de code en C++ pour illustrer le parsing d'une date à partir d'une chaîne de caractères. Assurez-vous d'inclure la bibliothèque <string>:

```C++
#include <string>

using namespace std;

int main() {
	string date = "12/31/2021"; // Chaîne de caractères contenant la date
	int day, month, year;

	sscanf(date.c_str(), "%d/%d/%d", &month, &day, &year); // Parsing de la date à l'aide de sscanf

	cout << "Jour: " << day << endl;
	cout << "Mois: " << month << endl;
	cout << "Année: " << year << endl;

	return 0;
}
```

Voici la sortie de ce programme:

```
Jour: 31
Mois: 12
Année: 2021
```

Ce snippet montre comment utiliser la fonctionsscanf pour extraire les valeurs de jour, mois et année de la chaîne de caractères et les stocker dans des variables.

## Deep Dive:

La fonctionsscanf que nous avons utilisée dans l'exemple ci-dessus est une fonction héritée du C, qui permet de lire une chaîne de caractères formatée pour extraire les valeurs souhaitées. Il existe également d'autres fonctions utiles pour le "parsing" de dates en C++, telles que `stoi` ou `atoi` pour convertir des chaînes de caractères en valeurs numériques.

De plus, il existe des bibliothèques tierces qui proposent des solutions plus élaborées pour le "parsing" de dates, comme la bibliothèque open-source Boost.Date_Time.

## Voir Aussi:

- [Documentation C++](https://en.cppreference.com/w/cpp/io/c/fscanf)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)