---
title:                "C#: Escrevendo para o erro padrão."
simple_title:         "Escrevendo para o erro padrão."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em C#?

Escrever para o erro padrão, ou *standard error* em inglês, pode ser uma tarefa útil ao desenvolver programas em C#. Ao escrever para o erro padrão, é possível receber mensagens de erro e depurar o código de forma mais eficiente. Além disso, escrever para o erro padrão também pode ajudar a melhorar a qualidade do código e a identificar possíveis problemas antes mesmo deles ocorrerem.

## Como fazer?

Para escrever para o erro padrão em C#, é possível utilizar o método `Console.Error.WriteLine()` seguido da mensagem que deseja exibir. Veja um exemplo abaixo:

```
Código C#:

Console.Error.WriteLine("Erro: Valor inválido inserido!");
```
```
Saída:

Erro: Valor inválido inserido!
```

Caso deseje incluir variáveis na mensagem de erro, basta utilizar o caractere `{}` para indicar o local da variável na string e depois adicionar os valores desejados separados por vírgula. Por exemplo:

```
Código C#:

string nome = "Ana";
int idade = 25;
Console.Error.WriteLine("O nome e a idade inseridos foram: {0}, {1}", nome, idade);
```
```
Saída:

O nome e a idade inseridos foram: Ana, 25
```

## Mergulho profundo

Além de utilizar o método `Console.Error.WriteLine()`, também é possível escrever para o erro padrão utilizando o fluxo `Console.Error`. Este fluxo funciona de forma semelhante ao `Console.Out`, mas é utilizado especificamente para mensagens de erro. Veja um exemplo abaixo:

```
Código C#:

using System;
using System.IO;

public class Program 
{
    public static void Main() 
    {
        Console.SetError(new StreamWriter("erros.txt"));
        Console.Error.WriteLine("Erro: O arquivo não foi encontrado!");
        Console.Error.WriteLine("Hora do erro: {0}", DateTime.Now);
    }
}
```
```
Saída:

O arquivo "erros.txt" será criado e terá o seguinte conteúdo:

Erro: O arquivo não foi encontrado!
Hora do erro: 2021-04-27 11:00:00 AM
```

## Veja também

- Documentação oficial da Microsoft sobre `Console.Error` (em inglês): <https://docs.microsoft.com/en-us/dotnet/api/system.console.error>
- Tutoriais C# - Escrevendo para o erro padrão: <https://www.tutorialspoint.com/csharp/csharp_standard_error.htm>
- Artigo sobre tratamento de erros em C#: <https://www.devmedia.com.br/tratamento-de-excecoes-em-c-sharp/23349>