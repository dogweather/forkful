---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Deletando caracteres que correspondem a um padrão em C#

## O que & Por quê?
Deletar caracteres que correspondem a um padrão é uma tarefa comum em programação; envolve remover caracteres específicos de uma string baseada em um padrão definido. Isso é útil para limpar dados, como remover símbolos especiais de números de telefone ou espaços extras em textos.  

## Como fazer 
Por exemplo, você pode usar o método `Replace` da classe `String` para substituir ocorrências de um caractere ou string por outra string. Para remover completamente um caractere, você substituiria por uma string em branco.

```C#
string textoOriginal = "A123B456C789D";
string textoLimpo = textoOriginal.Replace("123", "");
Console.WriteLine(textoLimpo); // Saída: "AB456C789D"
```
Nesse exemplo, "123" é o padrão que estamos buscando no `textoOriginal`, e substituímos todas as ocorrências pelo segundo argumento, que é uma string vazia, efetivamente removendo "123" do `textoOriginal`.

## Mergulho Profundo
Historicamente, essa é uma operação que você teria que fazer manualmente, iterando sobre cada caractere na string e construindo uma nova string sem os caracteres indesejados. Felizmente, a classe `String` em C# oferece um método conveniente `Replace` para realizar essa tarefa de forma fácil e eficiente. No entanto, vale lembrar que strings são imutáveis em C#, então toda operação que modifica uma string na verdade cria uma nova string.

Para padrões mais complexos, você pode usar expressões regulares com a classe `Regex`. 

```C#
string textoOriginal = "A123B456C789D";
string textoLimpo = Regex.Replace(textoOriginal, "[A-C]", "");
Console.WriteLine(textoLimpo); // Saída: "123456789D"
```
Lembre-se de que a manipulação de strings pode ser uma operação pesada, dependendo do tamanho de sua string e da frequência com que você executa essas operações. 

## Veja também
- [Documentação oficial de String.Replace](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace?view=net-6)
- [Guia Microsoft para expressões regulares em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expressions)