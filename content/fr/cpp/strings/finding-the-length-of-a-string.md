---
date: 2024-01-20 17:47:04.580716-07:00
description: "How to: Utilisez `std::string` et sa m\xE9thode `length()` pour obtenir\
  \ la taille facilement."
lastmod: '2024-03-13T22:44:58.151803-06:00'
model: gpt-4-1106-preview
summary: "Utilisez `std::string` et sa m\xE9thode `length()` pour obtenir la taille\
  \ facilement."
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## How to:
Utilisez `std::string` et sa méthode `length()` pour obtenir la taille facilement.

```C++
#include <iostream>
#include <string>

int main() {
    std::string texte = "Bonjour!";
    std::cout << "La longueur de la chaîne est: " << texte.length() << std::endl;
    return 0;
}

// Sortie:
// La longueur de la chaîne est: 8
```

Si vous manipulez des chaînes C-style (`char` arrays), utilisez `strlen` de `<cstring>`.

```C++
#include <iostream>
#include <cstring>

int main() {
    char texte[] = "Bonjour!";
    std::cout << "La longueur de la chaîne est: " << strlen(texte) << std::endl;
    return 0;
}

// Sortie:
// La longueur de la chaîne est: 8
```

## Deep Dive
Dans le passé, en C, on utilisait `strlen` qui parcourt la chaîne jusqu'au caractère nul pour déterminer la longueur. C'est une opération en temps linéaire O(n).

Avec C++, `std::string` stocke sa longueur, donc obtenir la longueur est une opération en temps constant O(1). Mais si vous utilisez encore des chaînes C, attention aux dépassements de tampons - c'est pourquoi on préfère `std::string`.

`std::string::size()` est un synonyme de `std::string::length()`. Utilisez celui que vous trouvez le plus clair. Pour les suites de caractères Unicode et autres besoins avancés, regardez du côté de bibliothèques spécialisées comme ICU.

## See Also
- [Documentation de `std::string::length`](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [Documentation de `strlen`](https://en.cppreference.com/w/c/string/byte/strlen)
- [International Components for Unicode (ICU)](http://site.icu-project.org/)
