---
title:    "C++: Concatenando strings"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que usar concatenação de strings?

Se você está familiarizado com programação em C++, provavelmente já se deparou com o conceito de concatenação de strings em algum momento. Mas por que exatamente alguém iria querer unir duas ou mais strings em uma única string? A resposta é simples: para criar uma nova string que combine as informações ou mensagens contidas nas strings originais. Isso pode ser útil em diferentes situações, como exibição de mensagens personalizadas ao usuário ou manipulação de dados em uma aplicação.

## Como fazer concatenação de strings em C++

A concatenação de strings é uma tarefa relativamente simples em C++. Vamos supor que queremos unir as strings "Hello" e "world" para formar a frase "Hello world". Podemos fazer isso da seguinte maneira:

```C++
#include <iostream>
#include <string>

int main() {
  // declaramos as duas strings que queremos unir
  std::string str1 = "Hello";
  std::string str2 = "world";
  
  // usamos o operador de adição (+) para concatenar as strings
  std::string frase = str1 + " " + str2;
  std::cout << frase << std::endl; // output: Hello world
  
  return 0;
}
```

No código acima, declaramos duas variáveis do tipo string - "str1" e "str2" - e atribuímos a elas os valores "Hello" e "world", respectivamente. Em seguida, usamos o operador de adição (+) para unir as duas strings, adicionando também um espaço em branco (" ") entre elas. Finalmente, imprimimos a nova string criada ("Hello world") na tela.

## Profundidade na concatenação de strings

Embora a concatenação de strings seja uma tarefa simples, é importante ter cuidado ao utilizá-la em seu código. Por exemplo, é importante garantir que as strings que estão sendo unidas estejam no formato correto, pois uma pequena diferença pode resultar em uma saída inesperada. Além disso, é possível que ocorram problemas de desempenho caso a concatenação seja feita com um grande número de strings ou em loops repetitivos.

Outro ponto a ser destacado é que, em C++, as strings são imutáveis, ou seja, não é possível modificá-las diretamente após sua criação. Portanto, na verdade, a concatenação de strings cria uma nova string, e a original permanece inalterada.

## Veja também

- [Tutorial de strings em C++ (em inglês)](https://www.programiz.com/cpp-programming/strings)
- [Documentação oficial do C++ (em inglês)](https://en.cppreference.com/w/cpp/string/basic_string)
- [Publicação sobre concatenação de strings em C++ (em português)](https://www.codepolitan.com/iniciando-com-cpp-concatenacao-de-strings-5b8faff7e9a22)