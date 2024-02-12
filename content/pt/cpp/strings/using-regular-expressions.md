---
title:                "Usando expressões regulares"
aliases:
- /pt/cpp/using-regular-expressions/
date:                  2024-02-03T19:16:08.913516-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Expressões regulares em C++ são sequências de caracteres que definem um padrão de busca, usados para a correspondência ou manipulação de strings. Programadores as utilizam em tarefas como validação de entrada, busca por ocorrências dentro de strings, ou divisão de strings em tokens, tornando-as uma ferramenta indispensável para o processamento de texto eficiente e eficaz.

## Como fazer:
O C++11 introduziu suporte para expressões regulares na biblioteca padrão, `<regex>`, oferecendo uma estrutura robusta para busca e manipulação de strings. Aqui está um exemplo básico de uso de expressões regulares para buscar um padrão dentro de uma string:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string alvo = "Olá, meu email é exemplo@exemplo.com";
    std::regex padrao_email(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(alvo, padrao_email)) {
        std::cout << "Email encontrado!" << std::endl;
    } else {
        std::cout << "Nenhum email encontrado." << std::endl;
    }

    return 0;
}
```
**Saída de Exemplo**
```
Email encontrado!
```

Para manipulações mais complexas, como substituir padrões dentro de strings, as expressões regulares do C++ podem ser muito úteis:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string texto = "A chuva na Espanha cai principalmente na planície.";
    std::regex regex_vogal("([aeiou])");

    std::string texto_substituido = std::regex_replace(texto, regex_vogal, "*");
    std::cout << texto_substituido << std::endl;

    return 0;
}
```
**Saída de Exemplo**
```
A ch*v* n* Esp*nh* c** pr*nc*p*lm*nt* n* pl*n*c**.
```

Para programadores explorando além da biblioteca padrão, a biblioteca Boost Regex (`boost/regex.hpp`) é uma opção de terceiros popular que oferece capacidades de regex aprimoradas e otimizações de desempenho, particularmente para padrões complexos ou extenso processamento de dados:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "As bibliotecas Boost são divertidas!";
    boost::regex expr("(\\w+)\\s(bibliotecas)"); // Correspondência "Boost bibliotecas"
    std::string fmt("GNU \\1"); // Substitui por "GNU Boost"

    std::string resultado = boost::regex_replace(s, expr, fmt);
    std::cout << resultado << std::endl;

    return 0;
}
```
**Saída de Exemplo**
```
GNU Boost são divertidas!
```

Estes exemplos arranham a superfície das capacidades do C++ com expressões regulares, ilustrando buscas básicas, correspondência de padrões e substituições, seja usando a biblioteca padrão ou aprimorado pela poderosa implementação de regex da Boost.
