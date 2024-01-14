---
title:                "C#: Escrevendo para o erro padrão"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o standard error em C#?

Escrever para o standard error pode ser útil para depurar o seu código e identificar possíveis erros e falhas durante a execução do programa. Ao redirecionar os erros para o standard error, você pode obter mais informações sobre o que está acontecendo no seu código e ajudar a identificar e corrigir problemas rapidamente.

## Como fazer isso em C#?

Para escrever para o standard error em C#, você pode utilizar o objeto "Console.Error" e o método "WriteLine()" para imprimir uma mensagem de erro no console. Aqui está um exemplo de código:

```C#
using System;

namespace escrevendoErros
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Error.WriteLine("Ocorreu um erro no programa.");
        }
    }
}
```

E a saída do programa será:

```
Ocorreu um erro no programa.
```

## Mais informações sobre escrever para o standard error

Além de imprimir mensagens de erro, você também pode utilizar o objeto "Console.Error" para gravar informações de depuração durante a execução do programa. Essas informações podem ser úteis para identificar a causa de um erro e corrigi-lo com mais precisão.

Além disso, vale ressaltar que ao escrever para o standard error, você também pode redirecionar essas informações para um arquivo de log, por exemplo. Isso pode facilitar a análise de erros e o monitoramento do seu programa.

## Veja também
- [Guias de referência para C#](https://docs.microsoft.com/pt-br/dotnet/csharp/)
- [Documentação oficial do Console (C#)](https://docs.microsoft.com/pt-br/dotnet/api/system.console?view=netcore-3.1)
- [Como usar a depuração em C#](https://docs.microsoft.com/pt-br/visualstudio/debugger/?view=vs-2019)