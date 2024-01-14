---
title:                "C++: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

Quando estamos lidando com strings em nossos programas, muitas vezes precisamos combinar ou juntar diferentes partes de texto para formar uma única string. A concatenação de strings é uma técnica fundamental na programação em C++ que nos permite fazer isso de forma eficiente e eficaz. Vamos ver como podemos realizar essa tarefa em nossos códigos.

## Como fazer

Para concatenar strings em C++, podemos usar o operador de adição (+) ou a função `concat` da biblioteca `string` padrão. Vamos dar uma olhada em alguns exemplos de como podemos usar esses métodos:

```
#include <iostream> 
#include <string> 
using namespace std; 

int main() { 
    string nome = "João"; 
    string sobrenome = "Silva"; 
    string nomeCompleto = nome + " " + sobrenome; 

    cout << "Nome completo: " << nomeCompleto << endl; 

    // Output: Nome completo: João Silva 

    return 0; 
}
```

```
#include <iostream> 
#include <string> 
using namespace std; 

int main() { 
    string saudacao = "Olá "; 
    string nome = "Maria"; 
    string mensagem = saudacao.concat(nome); 

    cout << mensagem << endl; 

    // Output: Olá Maria 

    return 0; 
}
```
Podemos ver que, em ambos os exemplos, podemos simplesmente usar o operador de adição (+) ou a função `concat` para combinar diferentes strings e armazenar o resultado em uma nova variável. Também podemos incluir caracteres especiais, como espaços em branco, para formatar a string final de acordo com nossas necessidades.

## Um mergulho mais profundo

Quando concatenamos strings em C++, é importante considerar o tipo de variáveis que estamos usando. Se estivermos lidando com strings de caracteres (variáveis `char`), devemos usar a função `strcat` em vez da função `concat`. Isso ocorre porque a função `concat` é projetada para trabalhar com o tipo `string` da biblioteca padrão, que é mais eficiente para manipular strings longas.

Outra dica importante é evitar concatenar muitas strings em um único comando. Isso pode causar a realocação de memória desnecessária e, portanto, afetar a eficiência do nosso código. Em vez disso, é recomendado concatenar pequenas partes de cada vez e armazenar o resultado em uma variável temporária até obtermos a string final desejada.

## Veja também

- [Documentação oficial do C++ sobre concatenação de strings](https://en.cppreference.com/w/cpp/string/basic_string/concat)
- [Artigo sobre manipulação de strings em C++](https://www.geeksforgeeks.org/strings-c-2/)
- [Tutorial em vídeo sobre concatenação de strings em C++](https://www.youtube.com/watch?v=rd8wtAYRabo)