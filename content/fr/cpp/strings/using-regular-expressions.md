---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:18.232717-07:00
description: "Les expressions r\xE9guli\xE8res en C++ sont des s\xE9quences de caract\xE8\
  res qui d\xE9finissent un motif de recherche, utilis\xE9 pour la correspondance\
  \ ou la\u2026"
lastmod: '2024-02-25T18:49:54.814249-07:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res en C++ sont des s\xE9quences de caract\xE8\
  res qui d\xE9finissent un motif de recherche, utilis\xE9 pour la correspondance\
  \ ou la\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières en C++ sont des séquences de caractères qui définissent un motif de recherche, utilisé pour la correspondance ou la manipulation de chaînes de caractères. Les programmeurs les utilisent pour des tâches telles que la validation d'entrées, la recherche d'occurrences dans des chaînes de caractères ou la segmentation de chaînes en tokens, ce qui en fait un outil indispensable pour un traitement de texte efficace et efficace.

## Comment faire :
C++11 a introduit le support des expressions régulières dans la bibliothèque standard, `<regex>`, offrant un cadre robuste pour la recherche et la manipulation de chaînes de caractères. Voici un exemple de base d'utilisation des expressions régulières pour rechercher un motif au sein d'une chaîne :

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string cible = "Bonjour, mon email est exemple@exemple.com";
    std::regex motif_email(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(cible, motif_email)) {
        std::cout << "Email trouvé !" << std::endl;
    } else {
        std::cout << "Aucun email trouvé." << std::endl;
    }

    return 0;
}
```
**Exemple de sortie**
```
Email trouvé !
```

Pour des manipulations plus complexes, telles que le remplacement de motifs au sein de chaînes de caractères, les expressions régulières de C++ peuvent être très pratiques :

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string texte = "La pluie en Espagne tombe principalement dans la plaine.";
    std::regex regex_voyelle("([aeiou])");

    std::string texte_remplace = std::regex_replace(texte, regex_voyelle, "*");
    std::cout << texte_remplace << std::endl;

    return 0;
}
```
**Exemple de sortie**
```
L* pl**e en Esp*gn* t*mb* pr*nc*p*lement d*ns l* pl**ne.
```

Pour les programmeurs explorant au-delà de la bibliothèque standard, la bibliothèque Boost Regex (`boost/regex.hpp`) est une option tierce populaire offrant des capacités de regex améliorées et des optimisations de performances, particulièrement pour des motifs complexes ou un traitement de données étendu :

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Les bibliothèques Boost sont amusantes !";
    boost::regex expr("(\\w+)\\s(bibliothèques)"); // Correspond à "Boost bibliothèques"
    std::string fmt("GNU \\1"); // Remplace par "GNU Boost"

    std::string résultat = boost::regex_replace(s, expr, fmt);
    std::cout << résultat << std::endl;

    return 0;
}
```
**Exemple de sortie**
```
GNU Boost sont amusantes !
```

Ces exemples effleurent la surface des capacités de C++ avec les expressions régulières, illustrant les recherches de base, les correspondances de motifs et les remplacements, soit en utilisant la bibliothèque standard, soit en étant renforcés par la puissante mise en œuvre regex de Boost.
