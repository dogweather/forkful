---
title:                "Concatenando strings"
html_title:           "PowerShell: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Concatenar strings é o processo de unir ou juntar duas ou mais strings em uma única string. Isso é comumente usado por programadores para criar strings mais complexas e significativas, ou para criar saídas de dados mais visíveis para os usuários. A ferramenta utilizada para realizar essa tarefa em PowerShell é o operador de concatenação "+". 

## Como fazer:

Para concatenar strings em PowerShell, é necessário usar o operador de concatenação "+" entre as strings que deseja unir. Aqui está um exemplo simples:

```
$string1 = "Hello"
$string2 = " World!"
$string3 = $string1 + $string2
```

Neste exemplo, a variável `$string1` contém a string "Hello" e a variável `$string2` contém a string " World!". A variável `$string3` é criada ao unir as duas strings usando o operador de concatenação, resultando em "Hello World!".

Você também pode usar o operador de atribuição composto "+=" para concatenar strings ao valor atual de uma variável. Aqui está um exemplo:

```
$string = "Hello"
$string += " World!"
```

Este código irá concatenar a string " World!" à string atual contida na variável `$string` resultando em "Hello World!".

## Detalhando:

### Contexto histórico:
Concatenar strings é um conceito amplamente usado na programação desde os primórdios da computação. Inicialmente, as linguagens de programação ofereciam métodos específicos para unir strings, mas com o avanço tecnológico e o surgimento de novas linguagens de programação, o operador de concatenação se tornou uma forma padrão de realizar essa tarefa.

### Alternativas:
Além do operador de concatenação "+", existem outras formas de unir strings em PowerShell. A função `Join()` pode ser usada para concatenar uma lista de strings separadas por um delimitador específico. Existe também o operador de substituição `-f`, que permite a inserção de valores em uma string formatada.

### Detalhes da implementação:
O operador de concatenação "+" é implementado em PowerShell usando o operador de adição do .NET. Ele é principalmente usado para concatenar strings, mas também pode ser usado para adicionar valores numéricos e outras variáveis. É importante lembrar que, ao unir uma string com um objeto diferente de uma string, PowerShell tentará converter o objeto em uma string antes de realizar a concatenação.

## Veja também:

- Documentação Microsoft sobre o operador de concatenação em PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_arithmetic_operators?view=powershell-7.1
- Artigo sobre operador de atribuição composto em PowerShell: https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-concatenating-strings/
- Tutorial sobre uso da função `Join()` em PowerShell: https://adamtheautomator.com/powershell-join/