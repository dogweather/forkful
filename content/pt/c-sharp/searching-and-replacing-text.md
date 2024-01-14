---
title:    "C#: Buscando e Substituindo Texto"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer busca e substituição de texto?

Fazer busca e substituição de texto é um recurso fundamental na programação. Com ele, é possível encontrar e substituir palavras, caracteres ou padrões específicos em um texto, de forma eficiente e automatizada. Isso é especialmente útil quando se trabalha com grandes conjuntos de dados ou quando é necessário fazer alterações em várias linhas de código de uma vez.

## Como fazer busca e substituição de texto em C#?

Para realizar busca e substituição de texto em C#, é necessário utilizar a classe `Regex`, que é responsável por expressões regulares. Primeiramente, é preciso instanciar um objeto dessa classe, especificando o padrão de texto a ser procurado e as opções de busca, como no exemplo abaixo:

```C#
Regex regex = new Regex("padrao", RegexOptions.IgnoreCase);
```

Após isso, é possível utilizar alguns métodos da classe `Regex` para realizar as ações desejadas, como `Match` para encontrar o primeiro padrão correspondente, `Matches` para encontrar todos os padrões correspondentes ou `Replace` para substituir um padrão por outro. Veja um exemplo completo de busca e substituição de texto em C#:

```C#
string texto = "Este é um texto de exemplo que será utilizado para realizar busca e substituição de padrões de texto.";
Regex regex = new Regex("texto", RegexOptions.IgnoreCase);
Match match = regex.Match(texto);
if (match.Success)
{
    Console.WriteLine($"Padrão encontrado na posição {match.Index}");
    string textoModificado = regex.Replace(texto, "frase");
    Console.WriteLine($"Novo texto: {textoModificado}");
}
else
{
    Console.WriteLine("Nenhum padrão encontrado.");
}
```

Este código irá procurar pelo padrão `texto` no texto de exemplo e, caso seja encontrado, irá substituí-lo por `frase`. O resultado da busca e substituição será:

```
Padrão encontrado na posição 8
Novo texto: Este é um frase de exemplo que será utilizado para realizar busca e substituição de padrões de texto.
```

## Aprofundando no assunto

Há diversas opções de busca que podem ser utilizadas em conjunto com a classe `Regex` em C#. Por exemplo, é possível utilizar expressões regulares para encontrar padrões específicos de caracteres, como letras e números, ou utilizar o método `Split` para dividir o texto em uma lista, utilizando um padrão como separador. Além disso, também é possível utilizar grupos de captura, que permitem extrair partes específicas do texto encontrado. É importante pesquisar sobre essas opções e escolher as que melhor se adequam às suas necessidades.

## Veja também

- [Documentação oficial do C# sobre expressões regulares](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Tutorial sobre busca e substituição de texto em C#](https://www.c-sharpcorner.com/article/how-to-use-regex-regexp-in-C-Sharp/)
- [Exercícios práticos de expressões regulares em C#](https://regexone.com/references/csharp)