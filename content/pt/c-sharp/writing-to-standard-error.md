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

## O que & Porquê?

Ao escrever um programa, é comum que os desenvolvedores queiram imprimir mensagens para informar o usuário sobre o que está acontecendo no código. No entanto, nem todas essas mensagens são importantes o suficiente para serem mostradas na saída padrão (standard output). É aqui que entra a escrita para o erro padrão (standard error). Esse método de impressão é usado para mostrar mensagens de erro e outros tipos de informação que podem ajudar os desenvolvedores a entenderem melhor o funcionamento do programa.

## Como fazer:

A maneira mais comum de escrever para o erro padrão em C# é usando o objeto "Console" e o método "Error". O seguinte trecho de código mostra como fazer isso:

```C#
Console.Error.WriteLine("Esta é uma mensagem de erro!");
```

O código acima irá imprimir a mensagem entre as aspas na saída de erro. Aqui está um exemplo de como o resultado pode aparecer:

```bash
Esta é uma mensagem de erro!
```

## Mergulho Profundo:

A escrita para o erro padrão é uma técnica amplamente utilizada em programação. Ela foi introduzida para ajudar os desenvolvedores a lidar com erros e outras questões do programa de forma mais eficaz. No entanto, é importante observar que existem alternativas para a escrita para o erro padrão. Uma opção é usar o log de eventos do sistema operacional, que pode ser útil para questões de monitoramento e depuração.

Falando em implementação, a escrita para o erro padrão é feita usando a interface de programação "Console". Este objeto é definido na biblioteca padrão de C# e fornece métodos para a entrada e saída de dados. O método "Error" é usado especificamente para escrever mensagens de erro na saída de erro.

## Veja também:

- [Documentação oficial do Console.Error](https://docs.microsoft.com/pt-br/dotnet/api/system.console.error?view=net-5.0)
- [Vídeo tutorial sobre como usar a saída de erro em C#](https://www.youtube.com/watch?v=c-wpWzmIOGo)