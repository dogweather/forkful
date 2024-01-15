---
title:                "Buscando e substituindo texto"
html_title:           "C#: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Procurar e substituir textos é uma tarefa muito comum em desenvolvimento de software. É importante saber como realizar essa ação de forma eficiente e precisa para economizar tempo e evitar erros no código.

## Como Fazer

Para procurar e substituir textos em C#, é preciso utilizar a classe String e seus métodos apropriados. Veja um exemplo:

```C#
string texto = "Olá, mundo!";
Console.WriteLine(texto.Replace("mundo", "Brasil"));
```

Este código irá imprimir "Olá, Brasil!" no console. Ao utilizar o método Replace, é possível especificar a palavra que deseja substituir e a nova palavra que será inserida no lugar.

Além disso, é possível utilizar expressões regulares para realizar substituições mais complexas. Por exemplo:

```C#
string texto = "Este texto contém a palavra código!";
Console.WriteLine(Regex.Replace(texto, @"\wcódigo\w", "palavra"));
```

Neste caso, será impresso "Este texto contém a palavra palavra!". A expressão regular utilizada (\wcódigo\w) irá procurar pela palavra "código" independente de onde ela se encontra na frase, substituindo-a por "palavra".

## Aprofundando

É importante entender que, ao utilizar o método Replace, todas as ocorrências da palavra serão substituídas. Porém, caso seja necessário substituir apenas uma ocorrência específica, é possível utilizar o método ReplaceFirst da classe Regex.

Além disso, a classe String possui outros métodos úteis para buscar e substituir textos, como o Contains, IndexOf e LastIndexOf. É recomendado consultar a documentação oficial da Microsoft para entender todas as funcionalidades disponíveis.

## Veja Também

- [Documentação oficial da Microsoft para a classe String](https://docs.microsoft.com/pt-br/dotnet/api/system.string?view=netcore-3.1)
- [Documentação oficial da Microsoft para a classe Regex](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)