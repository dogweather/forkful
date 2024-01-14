---
title:    "C#: Encontrando o tamanho de uma string"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Porque
Determinar o comprimento de uma string é uma das tarefas mais básicas e comuns que um programador C# irá enfrentar. Saber como encontrar o comprimento de uma string é uma habilidade fundamental que pode ser aplicada em uma variedade de projetos e problemas de programação.

## Como fazer
Para encontrar o comprimento de uma string em C#, podemos usar o método Length. Este método é muito simples de usar e retorna o número de caracteres na string fornecida. Vejamos um exemplo de código abaixo:

```C#
// Define uma string com o valor "Programação em C#" 
string str = "Programação em C#";

// Usa o método Length para encontrar o comprimento da string
int len = str.Length;

// Imprime o valor do comprimento da string
Console.WriteLine($"A string \"{str}\" tem um comprimento de {len} caracteres.");
```

A saída deste código será:

```
A string "Programação em C#" tem um comprimento de 18 caracteres.
```

Podemos ver que o método Length nos retorna o número correto de caracteres da string fornecida. Além disso, também podemos usar o método Length em strings vazias, que retornará um comprimento de 0.

## Aprofundando
Se nos aprofundarmos um pouco mais no funcionamento do método Length, podemos ver que ele na verdade retorna o número de unidades de código no objeto string.

Mas o que são unidades de código? Em resumo, cada caractere em uma string é composto de um ou mais bytes. Em alguns casos, 1 caractere pode ser representado por 2 ou mais bytes. O método Length nos dá o número de bytes necessários para armazenar a string, não necessariamente o número de caracteres visíveis.

Isso pode ser um pouco confuso, mas em geral, não precisamos nos preocupar com esses detalhes. O importante é saber que o método Length nos dá o número correto de caracteres em uma string, independentemente de como eles são armazenados na memória.

## Veja também
- [Documentação da Microsoft sobre o método Length](https://docs.microsoft.com/pt-br/dotnet/api/system.string.length?view=netcore-3.1)
- [Tutorial da Microsoft sobre strings em C#](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/)
- [Exercícios de prática para encontrar o comprimento de uma string em C#](https://www.w3resource.com/csharp-exercises/string/csharp-string-exercise-4.php)