---
title:    "C++: Excluindo caracteres que correspondem a um padrão"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existe uma função útil na linguagem de programação C++ que permite deletar caracteres que correspondam a um determinado padrão de texto. Mas por que alguém iria querer fazer isso? Bem, o uso dessa função pode ser benéfico em diferentes situações, como por exemplo, alterar determinadas palavras ou caracteres em um texto.

## Como Fazer

Para deletar caracteres seguindo um padrão em C++, é necessário utilizar a função `erase()` da biblioteca `string`. Primeiramente, é preciso criar uma variável do tipo `string` e atribuir a ela o texto em que será realizada a busca e a exclusão dos caracteres. Em seguida, utilizando a função `find()` é possível localizar a posição em que se encontra o padrão de texto a ser excluído.

Exemplo de código:

```
#include <iostream>
#include <string>
using namespace std;

int main() {
    // criando variável com o texto
    string texto = "Olá, meu nome é [João] e eu sou [programador]."; 
    
    // encontrando e excluindo o padrão de texto "[...]"
    int posicao = texto.find("["); // encontra o '[' na posição 19
    texto.erase(posicao, 7); // exclui o "[João]" da string
    
    // imprimindo o resultado
    cout << texto << endl; // saída: Olá, meu nome é e eu sou [programador].
    
    return 0;
}
```

Neste exemplo, foi encontrado e excluído o padrão de texto `[...]`, que pode ser modificado conforme a necessidade.

## Deep Dive

A função `erase()` também pode receber dois parâmetros, onde o primeiro indica a posição inicial e o segundo a quantidade de caracteres a serem excluídos. Isso possibilita uma maior precisão na exclusão de caracteres seguindo um padrão específico.

Outra opção é utilizar a função `replace()` em conjunto com a `find()` para substituir o padrão de texto por outro, ao invés de simplesmente excluí-lo. Essas funções podem ser úteis para fazer modificações em strings, como mudar a formatação de datas, moedas, entre outros.

## Veja também

- [Função `erase()` em C++](https://www.cplusplus.com/reference/string/string/erase/)
- [Método `find()` em C++](https://www.cplusplus.com/reference/string/string/find/)
- [Função `replace()` em C++](https://www.cplusplus.com/reference/string/string/replace/)