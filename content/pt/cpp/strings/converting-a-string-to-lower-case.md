---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:08.085325-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Converter uma string para minúsculas é o processo de transformar todos os caracteres alfabéticos de uma string para a sua forma minúscula. Programadores fazem isso para padronizar entradas de dados, facilitar comparações de texto e para atender requisitos de sistemas que diferenciam letras maiúsculas e minúsculas.

## Como Fazer:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string texto = "Olá, Mundo!";
    std::transform(texto.begin(), texto.end(), texto.begin(), 
                   [](unsigned char c){ return std::tolower(c); });
    
    std::cout << texto << std::endl; // saída: "olá, mundo!"
    return 0;
}
```

## Mergulho Profundo:

Antigamente, converter strings para minúsculas manualmente era um processo mais trabalhoso, pois exigia-se percorrer cada caractere e usar tabelas de conversão. Com C++ moderno, a biblioteca `<algorithm>` fornecer a função `std::transform` que torna isso um processo simples. Alternativamente, podem-se usar funções específicas, como `std::tolower`, em laços de repetição.

Um detalhe de implementação importante é que `std::tolower` requer o cabeçalho `<cctype>`. É essencial saber que o comportamento da função `std::tolower` pode ser indefinido se o caractere passado não for um caractere unsigned ou EOF. Isso é uma das razões pela qual a lambda acima usa `unsigned char`.

Existem também outras abordagens, como bibliotecas de terceiros, por exemplo, `boost::algorithm::to_lower` que podem oferecer mais flexibilidade e potência para certos casos de uso.

## Ver Também:

- Referência da C++ Standard Library para `std::transform`: http://www.cplusplus.com/reference/algorithm/transform/
- Referência da C++ Standard Library para `std::tolower`: http://www.cplusplus.com/reference/cctype/tolower/
- Documentação do Boost sobre manipulação de strings: https://www.boost.org/doc/libs/release/libs/algorithm/doc/html/the_boost_algorithm_library.html
