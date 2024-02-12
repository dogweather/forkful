---
title:                "Usando um shell interativo (REPL)"
aliases:
- /pt/c-sharp/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:12:22.488799-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?
Um REPL, ou Ciclo Ler-Avaliar-Imprimir, permite que você digite código C# e o execute interativamente. Programadores utilizam isso para experimentos rápidos, depuração ou aprender C#, sem a sobrecarga de configurar projetos completos.

## Como fazer:
Inicie um REPL no seu ambiente C# usando a janela Interativa C# ou execute `dotnet-script` no seu terminal. Aqui está um gostinho de como usá-lo:

```csharp
> var saudacao = "Olá, REPL!";
> Console.WriteLine(saudacao);
Olá, REPL!
>
```

Você recebe feedback instantaneamente. Sem etapas de compilação e execução. Apenas codifique e veja.

## Aprofundamento
O REPL viajou do Lisp para as linguagens modernas, prosperando em linguagens dinâmicas como Python. Com C#, o Roslyn aproximou o REPL dos desenvolvedores. `csi` para o Roslyn e `dotnet-script` para o .NET Core são opções sólidas. Um detalhe mais profundo: eles avaliam o código linha por linha, não tudo de uma vez, um modelo de execução diferente versus aplicativos C# típicos. Isso impacta a persistência de estado entre execuções e o escopo das variáveis.

A janela Interativa C# do Visual Studio é um REPL alimentado pelo Roslyn. Ela possui Intellisense, múltiplas referências e suporte a pacotes NuGet. Um grande avanço em relação aos primeiros experimentos de linha de comando.

Para linguagens alternativas, Python usa `IDLE`, JavaScript tem o REPL do Node.js, e F# vem com o `F# Interativo`. Cada um promove ciclos de feedback instantâneos, inestimáveis para testar pequenos trechos de código ou entender recursos da linguagem.

## Veja Também
- [REPL `dotnet-script` do .NET Core](https://github.com/filipw/dotnet-script)
