---
title:    "PHP: Encontrando o comprimento de uma string"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string é um processo fundamental em programação, pois permite que você obtenha informações importantes sobre os dados com os quais está trabalhando. Ao entender como determinar o comprimento de uma string, você pode melhorar suas habilidades em manipulação de dados e desenvolvimento de software.

## Como fazer?

A maneira mais simples de encontrar o comprimento de uma string em PHP é usando a função `strlen()`. Esta função aceita uma string como argumento e retorna o número de caracteres presentes nessa string. Veja um exemplo abaixo:

```PHP
$string = "Olá mundo!";
echo strlen($string); // Output: 11
```
Além disso, também é possível usar a propriedade `length` de uma string para obter seu comprimento. Veja outro exemplo abaixo:

```PHP
$string = "Lorem ipsum dolor sit amet";
echo $string.length; // Output: 26
```

## Profundidade

Ao trabalhar com diferentes tipos de dados, é importante entender como a linguagem de programação lida com cada um deles. Ao determinar o comprimento de uma string, você está essencialmente contando o número de caracteres presentes nessa string. No entanto, é importante notar que espaços em branco e caracteres especiais também são contados como caracteres. Isso pode afetar o resultado final do comprimento da string.

Outro ponto importante a ser observado é que o comprimento de uma string pode variar dependendo da codificação utilizada. Por exemplo, uma string em UTF-8 pode ter um comprimento diferente de uma string em UTF-16, pois cada caractere em UTF-8 é representado por um byte, enquanto em UTF-16 é representado por dois bytes.

Portanto, é importante ter cuidado ao lidar com diferentes codificações e considerar o uso da função `mb_strlen()` em vez de `strlen()` para obter um resultado mais preciso.

## Veja também
- Documentação oficial do PHP sobre a função `strlen()`: https://www.php.net/manual/pt_BR/function.strlen.php
- Documentação oficial do PHP sobre a função `mb_strlen()`: https://www.php.net/manual/pt_BR/function.mb-strlen.php