---
title:                "Encontrando o comprimento de uma string"
html_title:           "C#: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Encontrar o comprimento de uma string é uma tarefa comum na programação. String é um tipo de dado que armazena sequências de caracteres, e saber o tamanho dessa sequência é essencial para diversas operações. Os programadores geralmente precisam encontrar o comprimento de uma string para manipular e formatar textos, verificar a validade de entradas de usuário, entre outras necessidades.

## Como fazer:

É fácil encontrar o comprimento de uma string em C#, basta usar o método `Length`, que retorna um valor inteiro representando o número de caracteres presentes na string. Por exemplo:

```C#
string texto = "Hello world!";
Console.WriteLine(texto.Length);
```

Este código imprimirá o valor `12`, pois há 12 caracteres na string "Hello world!".

## Profundando:

O cálculo do comprimento de uma string tem uma longa história na programação. Em linguagens mais antigas, como C ou Assembly, era necessário percorrer cada caractere da string para determinar seu tamanho, o que era uma tarefa muito mais complexa. Atualmente, a maioria das linguagens de programação, incluindo C#, possuem métodos nativos para o cálculo do comprimento de strings.

Uma alternativa ao método `Length` em C# é o uso da função `strlen()` da biblioteca padrão de C, que é importada utilizando a diretiva `extern`. No entanto, isso só é necessário se sua aplicação estiver trabalhando com strings no formato de C, o que não é comum em C#.

Além disso, é importante notar que o método `Length` retorna o número de caracteres, não o número de bytes necessários para armazenar a string. Isso pode ser importante em situações de manipulação de dados binários, por exemplo.

## Veja também:

- Documentação oficial do método `Length` em C#: https://docs.microsoft.com/pt-br/dotnet/api/system.string.length?view=netcore-3.1

- Mais informações sobre a função `strlen()` em C: https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm