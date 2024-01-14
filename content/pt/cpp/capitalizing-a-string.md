---
title:    "C++: Capitalizar uma string"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar strings em C++

Em linguagens de programação como C++, as strings são sequências de caracteres usadas para armazenar e manipular dados de texto. Uma tarefa comum ao trabalhar com strings é capitalizar ou transformar em maiúsculas as letras nesta sequência. Isso pode ser útil ao criar nomes de usuário, títulos ou outras mensagens que precisam ser exibidas de forma mais elegante ou de acordo com regras gramaticais. Neste post, vamos discutir por que e como capitalizar strings em C++, além de mergulhar um pouco mais fundo no assunto.

## Como capitalizar strings em C++

Em C++, existem várias maneiras de capitalizar uma string, dependendo do resultado desejado. A seguir, mostraremos alguns exemplos de código e a saída esperada para cada um deles. Todos os exemplos assumem que o namespace `std` foi declarado no início do arquivo.

```
// Incluir bibliotecas necessárias
#include <iostream>
#include <string>
using namespace std;

int main(){

    // Definir a string a ser capitalizada
    string str = "ola, mundo!";

    // Exemplo 1: Usando a função `toupper` para transformar todas as letras em maiúsculas
    for(char& c : str) c = toupper(c);
    cout << str << endl; // Saída: OLA, MUNDO!

    // Exemplo 2: Usando a função `toupper` juntamente com um iterador
    transform(str.begin(), str.end(), str.begin(), ::toupper);
    cout << str << endl; // Saída: OLA, MUNDO!

    // Exemplo 3: Usando a função `capitalize` da biblioteca <boost/algorithm>
    #include <boost/algorithm/string.hpp>
    using namespace boost::algorithm;
    capitalize(str);
    cout << str << endl; // Saída: Ola, Mundo!
    
    return 0;
}

```

Como podemos ver nos exemplos acima, existem diferentes maneiras de capitalizar uma string em C++. O primeiro exemplo ilustra o uso de um laço for e a função `toupper`. Nesse caso, cada caractere da string é transformado em maiúsculo individualmente. O segundo exemplo utiliza a função `transform`, que recebe três argumentos: o início do intervalo, o final do intervalo e uma função que determina como transformar cada elemento. Por fim, no terceiro exemplo, utilizamos a função `capitalize` da biblioteca Boost, que também nos permite capitalizar a string diretamente.

## Mais informações sobre a capitalização de strings

Embora a capitalização de strings possa parecer uma tarefa simples, há alguns pontos a serem observados. Em primeiro lugar, o método utilizado para capitalizar a string pode variar dependendo do idioma ou contexto em que a string será utilizada. Alguns idiomas, por exemplo, possuem regras próprias para capitalizar nomes próprios ou outras palavras específicas. Além disso, é importante lembrar que a capitalização também pode alterar o comprimento da string, afetando o desempenho e a utilização de memória do programa.

Portanto, ao capitalizar uma string em C++, é importante considerar esses detalhes e escolher o método mais adequado para sua aplicação específica.

## Veja também

Aqui estão alguns links úteis para obter mais informações sobre o assunto:

- Documentação oficial da função `toupper` em C++: https://www.cplusplus.com/reference/cctype/toupper/
- Documentação oficial da função `transform` em C++: https://www.cplusplus.com/reference/algorithm/transform/
- Documentação da biblioteca Boost: https://www.boost.org/doc/libs/1_77_0/doc/html/string_algo/usage.html#idm46340116142576