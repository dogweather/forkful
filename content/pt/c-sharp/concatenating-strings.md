---
title:    "C#: Concatenando strings"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que alguém iria se preocupar em concatenar strings em C#. A resposta é simples: para combinar diferentes pedaços de texto em uma única sequência.

## Como fazer

Concatenar strings em C# é simples e pode ser feito de várias maneiras. Uma forma é usando o operador "+" para combinar duas strings, como no exemplo abaixo:

```C#
string saudacao = "Olá ";
string nome = "Maria";
string mensagem = saudacao + nome;

Console.WriteLine(mensagem); // Saída: Olá Maria
```

Outra maneira é usando o método `Concat` da classe `String`:

```C#
string primeiraPalavra = "Olá";
string segundaPalavra = "mundo";

string frase = String.Concat(primeiraPalavra, " ", segundaPalavra);

Console.WriteLine(frase); // Saída: Olá mundo
```

Também é possível usar a classe `StringBuilder` para concatenar strings de forma mais eficiente, especialmente se você estiver fazendo muitas operações de concatenação em um grande pedaço de texto.

## Deep Dive

Ao concatenar strings em C#, é importante lembrar que as strings são imutáveis, o que significa que elas não podem ser alteradas depois de criadas. Quando você "concatena" duas strings, na verdade está criando uma nova string com o conteúdo combinado das duas.

Por exemplo, ao usar o operador "+", se você estiver lidando com grandes pedaços de texto, é possível que muitas cópias desnecessárias das strings sejam criadas na memória, o que pode afetar a performance do seu programa. É por isso que usar a classe `StringBuilder` pode ser mais rápido e mais eficiente em termos de memória.

## See Also

Para saber mais sobre a concatenação de strings em C#, confira os links abaixo:

- [Documentação oficial do C# sobre strings](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/)
- [Tutorial sobre strings em C#](https://www.tutorialsteacher.com/csharp/csharp-string)
- [Artigo sobre a classe StringBuilder](https://www.c-sharpcorner.com/article/string-vs-stringbuilder-what-to-use-choose/)