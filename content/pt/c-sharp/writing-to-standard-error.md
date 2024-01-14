---
title:    "C#: Escrevendo no erro padrão"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em C#

Escrever para o erro padrão em C# pode parecer uma tarefa desnecessária para iniciantes na linguagem, mas na verdade é uma prática muito importante. Ao escrever para o erro padrão, você pode detectar e corrigir problemas em seu código de forma eficiente.

## Como fazer

Para escrever para o erro padrão em C#, você precisará utilizar a classe `Console`. Veja um exemplo abaixo:

```C#
Console.WriteLine("Mensagem de erro");
```

Neste exemplo, a mensagem "Mensagem de erro" será escrita no erro padrão, que por padrão é a tela do computador. Mas também é possível redirecionar o erro padrão para um arquivo ou outro dispositivo de saída, como mostrado abaixo:

```C#
Console.Error.WriteLine("Mensagem de erro");
```

Com isso, a mensagem será redirecionada para o dispositivo de saída especificado, que no caso é o erro padrão. Além disso, você também pode utilizar o método `Console.SetError()` para especificar o dispositivo de saída para o erro padrão.

## Mergulho profundo

A escrita para o erro padrão é especialmente útil durante o processo de depuração de código. Ao inserir mensagens de erro em seu código, você pode identificar exatamente em qual parte do código está ocorrendo um problema e corrigi-lo mais facilmente.

Também é possível customizar a formatação da mensagem de erro, utilizando placeholder como mostrado abaixo:

```C#
int idade = 18;
Console.Error.WriteLine($"Erro: idade inválida! Idade atual: {idade}");
```

Neste caso, ao utilizar o placeholder `{idade}`, a idade atual será incluída na mensagem de erro, facilitando a identificação do problema.

## Veja também

- [Documentação oficial do C#](https://docs.microsoft.com/pt-br/dotnet/csharp/)
- [Tutorial sobre escrita para o erro padrão em C#](https://www.tutorialspoint.com/csharp/csharp_errors.htm)
- [Vídeo explicativo sobre escrita para o erro padrão em C#](https://www.youtube.com/watch?v=muFh2v1qCtY)