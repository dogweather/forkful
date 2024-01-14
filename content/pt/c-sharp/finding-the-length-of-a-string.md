---
title:                "C#: Encontrando o comprimento de uma string"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Uma tarefa bastante comum na programação é a necessidade de determinar o comprimento de uma string. Isso pode ser útil para uma série de aplicações, como validação de entrada do usuário, formatação de saída ou manipulação de dados.

## Como fazer

Para encontrar o comprimento de uma string em C#, podemos utilizar o método `Length` da classe `String`. Veja um exemplo abaixo:

```C#
string texto = "Olá, mundo!";
int comprimento = texto.Length;

Console.WriteLine(comprimento); // output: 12
```

Neste exemplo, o valor da variável `comprimento` será igual a 12, já que a string possui 12 caracteres.

É importante notar que o método `Length` é sensível à diferença entre letras maiúsculas e minúsculas. Por exemplo, a string "abc" terá um comprimento de 3, enquanto "ABC" terá um comprimento de 4.

## Aprofundando

Encontrar o comprimento de uma string em C# pode parecer uma tarefa simples, mas por trás disso há vários conceitos e detalhes que podem ser explorados. Por exemplo, é possível utilizar o método `Length` em conjunto com outras funções da classe `String`, como o `Substring`, para obter informações mais precisas sobre a string.

Outro aspecto importante é que o método `Length` retorna o número de caracteres em uma string, e não o número de bytes. Isso significa que, em casos de strings com caracteres especiais ou acentos, o comprimento retornado pode ser diferente do esperado.

## Veja também

- Documentação oficial da Microsoft sobre o método `Length`: https://docs.microsoft.com/pt-br/dotnet/api/system.string.length?view=net-5.0
- Vídeo explicativo sobre encontrar o comprimento de uma string em C#: https://www.youtube.com/watch?v=fwvpKVXDMDE
- Exemplos de código em C# para manipulação de strings: https://github.com/muniap/awesome-csharp-string