---
title:                "C#: Imprimindo saída de depuração"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Porque

Antes de mergulharmos em como imprimir saída de debug em programas em C#, vamos primeiro entender por que é uma prática importante. A impressão de debug output permite que os programadores vejam informações úteis sobre o estado do programa durante a execução, o que pode ser essencial para detectar erros e realizar testes de depuração.

## Como Fazer

Aqui estão alguns exemplos de como você pode imprimir saída de debug em seus programas C#:

```C#
// Imprimindo uma mensagem simples
Console.WriteLine("Olá, mundo!");

// Imprimindo o valor de uma variável
int num = 10;
Console.WriteLine("O valor de num é: " + num);

// Imprimindo uma lista de valores
string[] nomes = { "Maria", "Pedro", "João" };
Console.WriteLine("Os nomes são: ");
foreach (string nome in nomes)
{
  Console.WriteLine(nome);
}
```

A saída desses exemplos seria:

```
Olá, mundo!
O valor de num é: 10
Os nomes são:
Maria
Pedro
João
```

## Deep Dive

Existem várias formas de imprimir saída de debug em C#, cada uma com suas próprias vantagens e desvantagens. O método `Console.WriteLine()` é amplamente utilizado por sua simplicidade e facilidade de uso. No entanto, existem outras opções, como o método `Debug.WriteLine()` da classe `System.Diagnostics`, que permite que você especifique uma categoria para a mensagem de debug, facilitando a identificação e filtragem de informações.

Além disso, é possível utilizar formatação de string para imprimir valores de variáveis de forma mais precisa e legível, em vez de simplesmente concatenar strings e valores. Isso pode ser feito utilizando o método `Console.WriteLine(string, object[])`, onde o primeiro argumento é a string de formatação e os demais são os valores a serem inseridos nessa string.

## Veja Também

- [Documentação da Classe Console em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.console?view=net-5.0)
- [Artigo sobre Debugging em C#](https://www.geeksforgeeks.org/debugging-in-c-sharp-step-by-step-guided-tour/)
- [Vídeo tutorial sobre impressão de debug em C#](https://www.youtube.com/watch?v=PGnyZMgbPk8)