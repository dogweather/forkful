---
title:                "Unindo strings"
html_title:           "C#: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes precisamos combinar várias strings em uma só, seja para construir uma frase, mensagem ou até mesmo para formatar dados. A concatenação de strings é uma funcionalidade importante na programação e pode facilitar muito o desenvolvimento de certas tarefas.

## Como Fazer

Usar a concatenação de strings em C# é bastante simples. Basta utilizar o operador "+" para unir as strings desejadas. Veja um exemplo abaixo:

```C#
string nome = "João";
string sobrenome = "Silva";
string nomeCompleto = nome + " " + sobrenome;
Console.WriteLine(nomeCompleto);
```

O resultado do código acima será "João Silva", pois as strings "nome" e "sobrenome" foram combinadas com um espaço em branco entre elas.

Também é possível usar a função Concat() da classe String para realizar a concatenação:

```C#
string cidade = "Rio";
string estado = "de Janeiro";
string local = String.Concat(cidade, " ", estado);
Console.WriteLine(local);
```

Neste caso, o resultado será "Rio de Janeiro". Além disso, é possível adicionar mais de duas strings na concatenação, basta separá-las com uma vírgula dentro dos parênteses da função Concat().

Outro ponto importante a ser lembrado é que os argumentos da função Concat() não precisam ser necessariamente strings. É possível combinar diferentes tipos de dados, como números e booleanos, por exemplo.

## Mergulho Profundo

Internamente, a concatenação de strings em C# é realizada através da criação de uma nova string, já que as strings são imutáveis na linguagem. Isso significa que cada vez que realizamos uma concatenação, o compilador cria uma nova string que contém a combinação das outras.

Isso pode causar um impacto na performance do código em casos em que existem muitas concatenações em sequência, pois a cada nova concatenação será necessária a alocação de memória para criar uma nova string. Para evitar esse problema, é recomendado o uso do tipo StringBuilder, que permite a realização de várias operações em uma única string sem necessidade de criar novas instâncias.

## Veja Também

- Documentação oficial sobre Concatenação de Strings em C#: https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/#concatenating-strings
- Tutorial sobre o uso do StringBuilder: https://www.eduardopires.net.br/2013/01/o-tipo-stringbuilder-no-csharp/
- Comparação entre concatenação de strings e StringBuilder: http://codigonoarduino.blogspot.com/2013/10/desempenho-stringbuilder-vs-string.html