---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever no erro padrão significa mandar mensagens de erro pro fluxo de saída específico pra erros, diferente do fluxo de saída padrão. Programadores fazem isso para separar erros dos dados normais de saída, facilitando diagnósticos e logging.

## Como fazer:
Vamos escrever uma mensagem simples para o erro padrão e uma para a saída padrão, pra ver a diferença.

```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Olá, mundo!"); // Saída padrão
        Console.Error.WriteLine("Ops, ocorreu um erro!"); // Erro padrão
    }
}
```

Quando executar isso, "Olá, mundo!" vai para a saída padrão e "Ops, ocorreu um erro!" para o erro padrão. Se tu redirecionar esses fluxos para arquivos, vais ver claramente a separação:

```shell
dotnet run > saida.txt 2> erro.txt
```

## Aprofundando
Historicamente, fluxos de saída e erro padrão vêm do UNIX, permitindo que mensagens de erro sejam separadas dos dados de saída. Alternativas incluem logging frameworks como log4net e NLog, que oferecem mais controle e opções. Na implementação, `Console.Error` é um `TextWriter` - o mesmo tipo de `Console.Out`, mas aponta para o fluxo de erro padrão.

## Veja também
- Documentação do .NET sobre a classe Console: [Console Class (System)](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-6.0)
- Um artigo sobre a diferença entre saída padrão e erro padrão: [Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
- Documentação sobre o log4net: [Apache log4net - Apache Logging Services](https://logging.apache.org/log4net/)
- Documentação do NLog: [NLog - Advanced and Structured Logging for Various .NET Platforms](https://nlog-project.org/documentation)
