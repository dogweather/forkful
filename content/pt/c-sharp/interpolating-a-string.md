---
title:                "Interpolando uma string"
date:                  2024-01-20T17:50:28.260077-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Interpolação de strings no C# permite injetar valores de variáveis dentro de uma string literal, melhorando a legibilidade e a manutenção do código. Programadores usam essa técnica para construir strings dinamicamente e evitar a concatenação manual, que pode se tornar confusa.

## Como Fazer:

```C#
// Exemplo básico de interpolação de strings
string nome = "João";
int idade = 30;
string mensagem = $"Olá, meu nome é {nome} e eu tenho {idade} anos.";
Console.WriteLine(mensagem);

// Saída: Olá, meu nome é João e eu tenho 30 anos.

// Interpolação com expressões
double preco = 99.99;
int quantidade = 2;
string recibo = $"O total é: {preco * quantidade:C}";
Console.WriteLine(recibo);

// Saída: O total é: R$ 199,98
```

## Aprofundando:

Antes do C# 6, concatenar strings com o operador `+` era comum, mas isso podia levar a erros e código difícil de ler. A interpolação de strings introduzida no C# 6 apresentou uma maneira elegante de incorporar expressões dentro de strings através do uso de chaves `{ }` prefixadas com um cifrão `$`. Esta funcionalidade é similar ao `printf` em C ou ao `.format` em Python, mas é mais integrada à linguagem C# e suporta todas as suas expressões.

Alternativas incluem o uso do `string.Format()`, que serve ao mesmo propósito mas em sintaxe menos direta. Outras possibilidades são o uso de `StringBuilder` para montagens mais complexas ou manipulações de strings que demandam alto desempenho.

Internamente, a interpolação de strings é transformada pelo compilador em uma chamada para `string.Format()`, portanto não existe um ganho de performance, no entanto, a clareza do código é significativamente melhorada.

## Veja Também:

- Documentação Oficial da Microsoft sobre interpolação de strings: [Interpolação de strings (Guia de C#)](https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/tokens/interpolated)
- Tutorial da Microsoft sobre como formatar strings em C#: [Formatação de tipos](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/formatting-types)
- Stack Overflow em Português, uma comunidade para programadores tirarem dúvidas: [Stack Overflow em Português](https://pt.stackoverflow.com/)
