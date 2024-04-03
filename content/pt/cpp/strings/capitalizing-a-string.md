---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:09.344406-07:00
description: "Capitalizar uma string envolve converter o caractere inicial de cada\
  \ palavra na string para mai\xFAscula, caso esteja em min\xFAscula, enquanto mant\xE9\
  m os demais\u2026"
lastmod: '2024-03-13T22:44:46.863249-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string envolve converter o caractere inicial de cada palavra\
  \ na string para mai\xFAscula, caso esteja em min\xFAscula, enquanto mant\xE9m os\
  \ demais caracteres inalterados."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
Em C++, você pode capitalizar uma string usando a biblioteca padrão sem a necessidade de bibliotecas de terceiros. No entanto, para comportamentos de capitalização mais complexos ou específicos, bibliotecas como Boost podem ser bastante úteis. Abaixo estão exemplos ilustrando ambas abordagens.

### Usando Biblioteca Padrão do C++:
```cpp
#include <iostream>
#include <cctype> // para std::tolower e std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Saída: "Hello World From C++"
}
```

### Usando Biblioteca Boost:
Para manipulações de strings mais avançadas, incluindo capitalização ciente de localidade, você pode querer usar a biblioteca Boost String Algo.

Primeiro, garanta que você tenha a biblioteca Boost instalada e configurada no seu projeto. Então você pode incluir os cabeçalhos necessários e usar seus recursos conforme mostrado abaixo.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // capitalize a primeira letra de cada palavra
    boost::algorithm::to_lower(capitalizedText); // garantindo que a string esteja em minúsculas
    capitalizedText[0] = std::toupper(capitalizedText[0]); // deixando o primeiro caractere maiúsculo

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // deixar maiúsculo depois de um espaço
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Saída: "Hello World From C++"
}
```

Neste caso, Boost simplifica algumas das tarefas de manipulação de strings, mas ainda requer uma abordagem personalizada para capitalização verdadeira, já que principalmente oferece utilitários de transformação e conversão de caso.
