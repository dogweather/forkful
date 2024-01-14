---
title:    "C++: Concaténation de chaînes de caractères"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une tâche courante en programmation, surtout en C++. Elle permet de fusionner plusieurs chaînes pour en former une seule, ce qui peut être utile dans de nombreuses situations, telles que l'affichage de messages personnalisés ou la manipulation de données.

## Comment faire

En C++, la concaténation de chaînes de caractères se fait en utilisant l'opérateur `+` ou la fonction `concat()`. Voici quelques exemples de code pour mieux comprendre :

```C++
// Utilisation de l'opérateur +
string nom = "Jean";
string prenom = "Dupont";
string message = "Bonjour " + prenom + " " + nom + " !";
cout << message; // affiche "Bonjour Jean Dupont !"
```

```C++
// Utilisation de la fonction concat()
string chaine1 = "Hello";
string chaine2 = " World!";
string resultat = concat(chaine1, chaine2);
cout << resultat; // affiche "Hello World!"
```

Il est important de noter que les chaînes de caractères doivent être du même type pour pouvoir être concaténées. Dans le cas contraire, une erreur de compilation sera générée.

En plus de l'opérateur `+` et de la fonction `concat()`, il est également possible d'utiliser la bibliothèque `<sstream>` pour concaténer des variables de différents types.

## Plongée en profondeur

En C++, les chaînes de caractères sont en fait des objets de la classe `string`. Ainsi, lorsqu'on les concatène, on utilise en fait les méthodes de cette classe. Par exemple, l'opérateur `+` appelle la méthode `append()` qui permet de concaténer deux chaînes. De même, la fonction `concat()` fait appel à cette même méthode.

De plus, la concaténation de chaînes de caractères peut également se faire avec des littéraux de chaînes, c'est-à-dire des chaînes de caractères écrites directement dans le code. Par exemple :

```C++
string message = "J'ai " + to_string(3) + " pommes."; // affiche "J'ai 3 pommes."
```

Dans ce cas, le compilateur effectue automatiquement la conversion entre le type `int` de la valeur `3` et le type `string` nécessaire pour la concaténation.

## Voir aussi

- [Documentation officielle de la classe string en C++](https://www.cplusplus.com/reference/string/string/)
- [Tutoriel vidéo (en français) sur la concaténation de chaînes de caractères en C++](https://www.youtube.com/watch?v=YSQgu1y7Ovk)