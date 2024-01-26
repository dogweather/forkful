---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Les expressions régulières, ou regex, servent à chercher des patterns dans du texte. Les programmeurs les utilisent pour valider, rechercher, et manipuler des données de façon efficace et concise.

## How to:
Utilisons `<regex>` pour match un numéro de téléphone:

```cpp
#include <iostream>
#include <string>
#include <regex>

int main() {
    std::string phone = "01-23-45-67-89";
    std::regex pattern(R"(^\d{2}-\d{2}-\d{2}-\d{2}-\d{2}$)");

    bool match = std::regex_match(phone, pattern);

    std::cout << (match ? "Valide" : "Non valide") << std::endl;

    return 0;
}
```

Output:
```
Valide
```

## Deep Dive
Les regex sont nés dans les années 1950. Alternatives : `std::string::find`, parsers. Les regex en C++ utilisent la bibliothèque `<regex>`, introduite en C++11.

## See Also
- Documentation C++ sur les regex: [cppreference.com](https://en.cppreference.com/w/cpp/regex)
- Testeur de regex : [regex101.com](https://regex101.com/)
- Tutoriel de regex : [regular-expressions.info](https://www.regular-expressions.info/tutorial.html)
