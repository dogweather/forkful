---
title:    "C#: Encontrando o comprimento de uma sequência"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que

Ao escrever código em C#, muitas vezes, precisamos manipular strings. Ao saber o comprimento de uma string, podemos realizar várias operações, como verificar se ela é vazia, percorrer cada caractere ou até mesmo realizar comparações entre duas strings. Portanto, encontrar o comprimento de uma string é uma habilidade fundamental para qualquer programador em C#.

## Como Fazer

Para encontrar o comprimento de uma string em C#, podemos usar o método `Length`, presente na classe `String`. Veja um exemplo abaixo:

```C#
using System;

class Program
{
    static void Main()
    {
        // declaração da string
        string minhaString = "Meu melhor amigo é o meu cachorro";

        // chamando o método Length
        int comprimento = minhaString.Length;

        // escrevendo o comprimento no console
        Console.WriteLine(comprimento);
    }
}
```

Neste exemplo, a saída será `32`, pois a string contém 32 caracteres, incluindo espaços e pontuação. Além disso, também é possível armazenar o valor do comprimento em uma variável para usá-lo posteriormente em seu código.

## Deep Dive

É importante ressaltar que o método `Length` retorna o número de caracteres existentes na string, incluindo espaços em branco e caracteres especiais. Ou seja, não é uma simples contagem de letras. Além disso, ele também pode ser usado em strings vazias, retornando o valor de `0`.

Outra coisa interessante sobre o `Length` é que ele é uma propriedade, e não um método. Isso significa que não precisamos usar parênteses para chamá-lo, apenas o nome da string seguido do operador ponto (`.`) e a palavra "Length".

## Veja Também

- Documentação Oficial do Método `Length` em C#: https://docs.microsoft.com/pt-br/dotnet/api/system.string.length?view=net-5.0
- Diferença entre Propriedades e Métodos em C#: https://pt.stackoverflow.com/questions/352502/qual-a-diferen%C3%A7a-entre-um-m%C3%A9todo-e-uma-propriedade-em-c