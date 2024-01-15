---
title:                "Escrevendo para o erro padrão"
html_title:           "C#: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para o erro padrão é uma prática útil para testar e depurar seu código. Ao escrever para o erro padrão, você pode visualizar informações importantes sobre erros e exceções que ocorrem durante a execução do seu programa.

## Como Fazer

Primeiro, você precisa entender o que é o erro padrão. Em C#, o erro padrão é um fluxo de saída que é usado para exibir mensagens de erro e informações de depuração. Para gravar mensagens para o erro padrão, você pode usar o método `Console.Error.WriteLine`.

Veja um exemplo simples de como escrever para o erro padrão em C#:

```
C# Console.Error.WriteLine("Este é um exemplo de mensagem de erro.");
```

O resultado deste código será a impressão da mensagem "Este é um exemplo de mensagem de erro." no console. Você também pode utilizar formatação de string para adicionar informações adicionais:

```
C# string nome = "João";
Console.Error.WriteLine("Olá, {0}. Seu nome possui {1} letras.", nome, nome.Length);
```

O resultado deste código será a impressão da mensagem "Olá, João. Seu nome possui 4 letras." no console.

## Aprofundando

Além do método `Console.Error.WriteLine`, existem outras formas de escrever para o erro padrão em C#. Você também pode utilizar a classe `StreamWriter` para gravar informações para o erro padrão.

Outro ponto importante é que, ao usar o erro padrão, as mensagens serão imediatamente impressas no console, independentemente de onde o código está sendo executado. Isso pode ser útil para rastrear erros em aplicações multithread ou serviços.

Se você estiver trabalhando em um projeto maior, uma boa prática é criar uma classe de registro de erros personalizada. Desta forma, você pode organizar melhor suas mensagens de erro e adicionar informações adicionais, como data e hora, número de linha ou nome do método.

## Veja Também

- Documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/api/system.console.error
- Diferença entre erro padrão e erro de rede: https://stackoverflow.com/questions/183202/difference-between-standard-error-and-error-from-dos-command-line
- Tutorial de como criar uma classe de registro de erro: https://exceptionnotfound.net/basics-csharp-custom-exception-classes/#creating-a-custom-exception-class-in-c