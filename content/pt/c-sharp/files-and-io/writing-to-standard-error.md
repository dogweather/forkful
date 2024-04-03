---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:42.001160-07:00
description: "Como fazer: Em C#, escrever no erro padr\xE3o pode ser alcan\xE7ado\
  \ usando o stream `Console.Error`. Este stream \xE9 usado especificamente para mensagens\
  \ de erro\u2026"
lastmod: '2024-03-13T22:44:46.600111-06:00'
model: gpt-4-0125-preview
summary: "Em C#, escrever no erro padr\xE3o pode ser alcan\xE7ado usando o stream\
  \ `Console.Error`."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## Como fazer:
Em C#, escrever no erro padrão pode ser alcançado usando o stream `Console.Error`. Este stream é usado especificamente para mensagens de erro e diagnósticos. Aqui está um exemplo básico:

```csharp
Console.Error.WriteLine("Erro: Falha ao processar a solicitação.");
```

Saída de exemplo (para stderr):
```
Erro: Falha ao processar a solicitação.
```

Para cenários onde você pode estar usando uma biblioteca de terceiros que oferece capacidades avançadas de registro, como `Serilog` ou `NLog`, você pode configurar essas bibliotecas para escrever registros de erro no stderr. Enquanto estes exemplos focam na simples redireção do console, lembre-se que em aplicações de produção, frameworks de registro oferecem opções muito mais robustas de tratamento de erro e saída. Aqui está um exemplo simples com `Serilog`:

Primeiro, instale o pacote Serilog e seu sink de Console:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Em seguida, configure o Serilog para escrever no stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Esta é uma mensagem normal.");
Log.Error("Esta é uma mensagem de erro.");
```

Saída de exemplo (para stderr para a mensagem de erro):
```
[15:04:20 ERR] Esta é uma mensagem de erro.
```

Nota: A configuração `standardErrorFromLevel` no sink de console do Serilog redireciona todos os eventos de log no nível especificado (Erro, neste caso) ou mais alto para o stream de erro padrão, enquanto mensagens de níveis inferiores como Informação são escritas no stream de saída padrão.
