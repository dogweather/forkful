---
title:    "C#: Escrevendo para o erro padrão"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para erro padrão?

Escrever para o erro padrão, ou standard error, é uma técnica importante na programação. Permite que os erros e mensagens de debug sejam exibidos de forma clara para o desenvolvedor, facilitando a identificação e correção de problemas no código.

## Como fazer

Para escrever para o erro padrão em C#, podemos utilizar o método `Console.Error.WriteLine()`, que envia uma mensagem para a saída de erro. Veja um exemplo abaixo:

```C#
Console.Error.WriteLine("Erro: não foi possível conectar ao banco de dados.");
```

Isso irá exibir no console a mensagem "Erro: não foi possível conectar ao banco de dados" na cor vermelha, indicando que é uma mensagem de erro.

Outra forma de escrever para o erro padrão é utilizando o `Console.SetError()` para redirecionar a saída para um arquivo de log, por exemplo. Isso pode ser útil em casos onde é necessário salvar o log de erros para análise posteriormente.

## Uma análise mais profunda

É importante mencionar que, ao escrever para o erro padrão, estamos lidando com saída de texto sem formatação. Isso significa que é responsabilidade do desenvolvedor garantir que as mensagens sejam claras e fáceis de entender.

Além disso, é importante lembrar que a saída de erro é diferente da saída padrão `Console.WriteLine()`. Enquanto a saída padrão envia mensagens para o console de forma geral, a saída de erro é destinada apenas para mensagens de erro e debug.

## Veja também

- [Documentação oficial da Microsoft sobre o método Console.Error.WriteLine()](https://docs.microsoft.com/pt-br/dotnet/api/system.console.error.writeline)
- [Artigo da DevMedia sobre a saída de erro em C#](https://www.devmedia.com.br/utilizando-a-saida-de-erro-no-c/22108)
- [Vídeo tutorial sobre a saída de erro no canal Programação Dinâmica](https://www.youtube.com/watch?v=xS7NBQvyQys)