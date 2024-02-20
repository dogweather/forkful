---
date: 2024-01-26 03:38:07.787392-07:00
description: "Stripar aspas de uma string significa remover aquelas inc\xF4modas aspas\
  \ duplas ou simples que envolvem nosso texto (' ou \"). Programadores frequentemente\u2026"
lastmod: 2024-02-19 22:05:05.927701
model: gpt-4-0125-preview
summary: "Stripar aspas de uma string significa remover aquelas inc\xF4modas aspas\
  \ duplas ou simples que envolvem nosso texto (' ou \"). Programadores frequentemente\u2026"
title: Removendo aspas de uma string
---

{{< edit_this_page >}}

## O que é & Por quê?
Stripar aspas de uma string significa remover aquelas incômodas aspas duplas ou simples que envolvem nosso texto (' ou "). Programadores frequentemente fazem isso para higienizar a entrada, armazenar texto em um banco de dados, ou preparar strings para mais processamentos sem o embaraço das aspas.

## Como fazer:
Aqui vai uma maneira direta de se livrar dessas aspas em C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Olá, 'Mundo'!")";
    std::string sem_aspas = remove_quotes(original);
    std::cout << sem_aspas << std::endl;
    return 0;
}
```

Execute isso, e você obterá:

```
Olá, Mundo!
```

Voilà! As aspas desapareceram.

## Aprofundando
Aspas têm sido uma chateação textual desde o início da computação. Antigamente, você via programadores laboriosamente passando por cada caractere para filtrar essas aspas. Hoje, nós temos `std::remove` na Standard Template Library (STL) para fazer o trabalho pesado.

Alternativas? Claro! Você poderia usar expressões regulares com `std::regex` para mirar nas aspas, mas isso é meio que usar um martelo para quebrar uma noz - poderoso, mas pode ser exagero para tarefas simples. Para aqueles que preferem sabores recentes de C++, vocês podem experimentar com `std::string_view` para abordagens não modificadoras.

Em termos de implementação, lembre-se que `std::remove` não remove elementos do container; ele reorganiza os elementos não removidos para frente e retorna um iterador para o novo fim do intervalo. É por isso que precisamos do método `erase` para cortar a cauda indesejada.

## Veja Também
- Referência do C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Mais sobre manipulação de `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
