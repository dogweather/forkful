---
title:                "Convertendo uma string para minúsculas"
html_title:           "C++: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e Porque?

Converter uma string para letras minúsculas é um processo em que todas as letras maiúsculas em uma string são transformadas em letras minúsculas. Isso é frequentemente feito por programadores para ajudar a padronizar o texto e torná-lo mais fácil de ser comparado e manipulado.

## Como fazer:

Para converter uma string para letras minúsculas em C++, podemos usar a função ```tolower()``` do cabeçalho ```<cctype>```. Aqui está um exemplo de código:

```
#include <iostream>
#include <cctype>
using namespace std;

int main() {
  string texto = "Ola Mundo!";
  for (int i = 0; i < texto.length(); i++) {
    texto[i] = tolower(texto[i]);
  }
  cout << texto << endl;
  return 0;
}
```

A saída deste código será "ola mundo!". Podemos ver que todas as letras maiúsculas foram convertidas para minúsculas.

## Mergulho Profundo:

Esta não é a única maneira de transformar uma string em letras minúsculas. Outra opção é usar a função ```transform()``` do cabeçalho ```<algorithm>```. Aqui está um exemplo de como usá-lo:

```
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

int main() {
  string texto = "Ola Mundo!";
  transform(texto.begin(), texto.end(), texto.begin(), ::tolower);
  cout << texto << endl;
  return 0;
}
```

A saída também será "ola mundo!". Além disso, é importante notar que ambas as funções também podem lidar com caracteres acentuados e outras línguas além do inglês.

## Veja também:

- Para mais informações sobre as funções ```tolower()``` e ```transform()```, consulte a [documentação do C++](https://www.cplusplus.com/).

- Se você estiver trabalhando com strings em outros idiomas, pode ser útil saber mais sobre [codificação de caracteres](https://pt.wikipedia.org/wiki/Codificação_de_caracteres).

Com essas informações, agora você pode facilmente transformar suas strings em letras minúsculas em C++ e usá-las em suas aplicações. Experimente e veja como isso pode facilitar suas tarefas de programação!