---
title:                "Localizando e substituindo texto."
html_title:           "C#: Localizando e substituindo texto."
simple_title:         "Localizando e substituindo texto."
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Porquê?
A busca e substituição de texto é um processo comum na programação, que consiste em procurar por uma determinada sequência de caracteres em um texto e substituí-la por outra. Os programadores fazem isso para automatizar o processo de alteração de múltiplas ocorrências de um determinado termo, economizando tempo e esforço.

## Como fazer:
Há várias maneiras de realizar busca e substituição em C#, mas uma das mais comuns é utilizando a função "Replace". Veja um exemplo de como substituir todas as ocorrências de um termo por outro em uma string:
```C#
string texto = "Preciso substituir todas as ocorrências desta palavra.";
string novoTexto = texto.Replace("ocorrências", "ocasiões");
Console.WriteLine(novoTexto);
```
**Saída:**
> Preciso substituir todas as ocasiões desta palavra.

## Mergulho profundo:
A busca e substituição de texto existe desde os primórdios da programação, sendo uma das tarefas mais básicas e úteis para os desenvolvedores. Além da função "Replace", existem outras formas de realizar esse processo em C#, como utilizando expressões regulares e bibliotecas específicas para esse fim, como o pacote "System.Text.RegularExpressions". É importante lembrar que, dependendo do tamanho do texto e da complexidade da substituição, pode ser necessário otimizar o código para melhorar a performance.

## Veja também:
- [Documentação oficial da função "Replace" em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace)
- [Tutorial sobre busca e substituição em C#](https://www.tutorialspoint.com/program-to-understand-replace-function-in-c-sharp)