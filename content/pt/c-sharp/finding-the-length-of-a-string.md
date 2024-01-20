---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Operações com strings em C#: Como encontrar o comprimento de uma string

## O Que & Por Quê?
Encontrar o comprimento de uma string em linguagens de programação, como C#, é identificar o número de caracteres que uma string contém. Isso é útil quando precisamos manipular ou validar strings, por exemplo, checar se uma string atende a uma certa quantidade de caracteres.

## Como Fazer:
No C#, usamos a propriedade `Length` para obter o comprimento de uma string. Aqui está um exemplo simples:

```C#
string texto = "Programar é divertido";
Console.WriteLine("O comprimento da string é: " + texto.Length);
```

Na saída, verá isto:

```
O comprimento da string é: 20
```
Esta é a maneira padrão de encontrar o comprimento de uma string em C#.

## Deep Dive
Historicamente, operar com strings tem sido um aspecto essencial em quase todas as linguagens de programação e continua a sê-lo com a evolução contínua dessas linguagens. No entanto, muitas vezes o método varia de uma linguagem para outra.

No contexto do C#, a propriedade `Length` foi introduzida para simplificar o processo e torná-lo mais eficiente. 

Uma alternativa ao uso da propriedade `Length` seria fazer um loop na string e contar os caracteres um a um. No entanto, essa é uma abordagem menos eficiente e deve ser evitada se possível.

Quanto à implementação, toda vez que uma string é criada em C#, a linguagem aloca memória para armazenar a string e seu comprimento. Quando a propriedade `Length` é acessada, ela simplesmente retorna o valor previamente armazenado, tornando a operação extremamente rápida.

## Veja Também 
Para saber mais sobre strings em C# e como elas podem ser manipuladas, os seguintes links serão úteis:

- Documentação oficial da Microsoft sobre Strings: [https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/)
- Tutorial detalhado sobre a classe string em C#: [https://www.tutorialsteacher.com/csharp/csharp-string](https://www.tutorialsteacher.com/csharp/csharp-string)
- Discussão em profundidade sobre a eficiência das operações em string no StackOverflow: [https://stackoverflow.com/questions/4483886/how-do-i-get-a-string-length-in-net-without-using-the-length-property](https://stackoverflow.com/questions/4483886/how-do-i-get-a-string-length-in-net-without-using-the-length-property)