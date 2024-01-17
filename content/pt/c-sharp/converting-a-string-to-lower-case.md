---
title:                "Convertendo uma string para letra minúscula"
html_title:           "C#: Convertendo uma string para letra minúscula"
simple_title:         "Convertendo uma string para letra minúscula"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e Por que?

Converter uma string para letras minúsculas é um processo que transforma todas as letras maiúsculas em letras minúsculas. Isso é importante para que o programa possa procurar e comparar strings com precisão, pois letras maiúsculas e minúsculas são consideradas diferentes. Os programadores geralmente fazem isso para tornar seus códigos mais robustos e evitar erros causados por diferenças de capitalização.

## Como Fazer:

```
// Código de exemplo para converter uma string para letras minúsculas
string frase = "EXEMPLO DE STRING"; // string em letras maiúsculas
string resultado = frase.ToLower(); // converter para letras minúsculas

Console.WriteLine(resultado); // saída: exemplo de string
```

## Mergulho Profundo:

Historicamente, o uso de letras maiúsculas e minúsculas no processo de codificação data da época em que as máquinas de datilografia eram usadas para inserir o código. Como as máquinas de datilografia tinham apenas letras maiúsculas, o hábito de usar letras maiúsculas para representar palavras-chave ou comandos se manteve até hoje. Existem alternativas para converter strings para letras minúsculas, como usar um método que verifica se duas strings são iguais independentemente da capitalização. A implementação dessa conversão geralmente envolve a utilização de uma tabela de mapeamento para substituir as letras maiúsculas pelas letras minúsculas correspondentes.

## Veja Também:

- Documentação oficial do método ToLower em C#: [https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- Tutorial sobre conversão de strings para letras minúsculas em C#: [https://www.tutorialspoint.com/csharp/csharp_string_tolower.htm](https://www.tutorialspoint.com/csharp/csharp_string_tolower.htm)
- Discussão sobre os prós e contras da conversão de strings para letras minúsculas em fóruns de programação: [https://stackoverflow.com/questions/21409709/to-lower-vs-to-lower-in-c-sharp-performance-gain-or-just-good-practice](https://stackoverflow.com/questions/21409709/to-lower-vs-to-lower-in-c-sharp-performance-gain-or-just-good-practice)