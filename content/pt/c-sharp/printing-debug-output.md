---
title:                "Imprimindo Saída de Depuração"
html_title:           "C#: Imprimindo Saída de Depuração"
simple_title:         "Imprimindo Saída de Depuração"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, durante o processo de desenvolvimento de um programa em C#, pode ser útil imprimir informações de depuração para verificar se o código está funcionando corretamente. Isso pode ser especialmente útil quando surgem erros ou comportamentos inesperados no programa.

## Como fazer

Para imprimir mensagens de depuração em C#, você pode usar o método `Console.WriteLine()`. Ele aceita um argumento de string que será impresso no console. Por exemplo:

```C#
string nome = "João";
int idade = 25;
Console.WriteLine("O nome é {0} e a idade é {1}", nome, idade);
```

Isso imprimirá a seguinte saída no console:

```
O nome é João e a idade é 25
```

Outra opção é usar o método `Debug.WriteLine()`, que é fornecido pela classe `System.Diagnostics`. Ele funciona de maneira semelhante ao `Console.WriteLine()` mas também permite definir pontos de interrupção no código para que a mensagem de depuração seja exibida quando o programa atinge esses pontos. Por exemplo:

```C#
string nome = "Maria";
int idade = 30;
Debug.WriteLine("O nome é {0} e a idade é {1}", nome, idade);
```

Ao executar o programa em modo de depuração, a mensagem será exibida no console ou no painel de saída do seu ambiente de desenvolvimento integrado (IDE) quando for atingido o ponto de interrupção.

## Deep Dive

Quando for imprimir informações de depuração, é importante considerar quais dados serão relevantes para o problema em questão. Você pode optar por imprimir valores de variáveis, mensagens de erro ou até mesmo adicionar formatação aprimorada para facilitar a leitura. Além disso, é importante lembrar de remover essas mensagens de depuração antes de finalizar o programa, pois elas podem afetar o desempenho e a segurança em uma versão de produção.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como imprimir debug output em C#:

- [Documentação oficial do Console.WriteLine](https://docs.microsoft.com/pt-br/dotnet/api/system.console.writeline)
- [Documentação oficial do Debug.WriteLine](https://docs.microsoft.com/pt-br/dotnet/api/system.diagnostics.debug.writeline)
- [Artigo sobre como usar mensagens de debug em C#](https://www.exceptionnotfound.net/debugging-in-c-sharp-caret-position-explained/)