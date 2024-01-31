---
title:                "Registro de Logs"
date:                  2024-01-26T01:01:19.142651-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"

category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/logging.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Logging (registro de logs) é o processo de gravar os eventos da aplicação e a saída de dados durante a execução. Programadores realizam registros de logs para diagnosticar erros, monitorar o desempenho do software, acompanhar ações dos usuários e manter a conformidade com padrões de segurança e negócios.

## Como fazer:
Em C#, você pode usar o espaço de nome integrado `System.Diagnostics` ou bibliotecas de terceiros como NLog ou log4net. Aqui está um exemplo rápido usando a interface `ILogger` disponível no .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Esta é uma mensagem informativa.");
        logger.LogWarning("Esta é uma mensagem de aviso.");
        logger.LogError("Esta é uma mensagem de erro.");
    }
}
```

Saída de exemplo:
```
info: Program[0]
      Esta é uma mensagem informativa.
warn: Program[0]
      Esta é uma mensagem de aviso.
fail: Program[0]
      Esta é uma mensagem de erro.
```

## Aprofundando
A história do registro de logs no desenvolvimento de software é quase tão antiga quanto a programação em si; evoluiu de simples instruções de impressão para sistemas sofisticados e configuráveis. Originalmente, logs eram feitos escrevendo em arquivos ou no console, mas isso cresceu para incluir estruturas mais complexas como sistemas de agregação de logs e plataformas de rastreamento distribuído (como a pilha ELK ou Jaeger).

Alternativas para o registro de logs incorporado no .NET incluem bibliotecas de terceiros:
- **NLog**: versátil e fácil de configurar, com muitos recursos para roteamento, formatação e filtragem de logs.
- **log4net**: inspirado pela biblioteca Java log4j, é altamente configurável a partir de XML e suporta uma variedade de repositórios de logs.

Quando se trata de detalhes de implementação, a escolha da sua abstração de registro de logs (como Microsoft.Extensions.Logging) e do provedor de logs subjacente pode afetar significativamente o desempenho e a confiabilidade da sua aplicação. É crucial configurar os níveis de log apropriadamente e garantir que a escrita dos logs não se torne um gargalo.

Além disso, o logging estruturado - onde você registra não apenas strings, mas pares chave-valor ou objetos - permite logs mais precisos e acionáveis, que são mais fáceis de consultar e analisar.

## Veja Também
- [Documentação do Microsoft.Extensions.Logging](https://docs.microsoft.com/pt-br/aspnet/core/fundamentals/logging/)
- [Documentação do NLog](https://nlog-project.org/documentation/)
- [Documentação do log4net](https://logging.apache.org/log4net/)
- [Documentação do Serilog](https://serilog.net/) (para um exemplo de logging estruturado)
