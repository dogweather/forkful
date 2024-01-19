---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

**## O quê e Por quê?**

Extrair substrings é uma operação comum em programação que envolve a retirada de uma parte de uma string. Nós, programadores, fazemos isso para manipular dados de texto de maneira mais precisa, como dividir nomes de usuários ou analisar o conteúdo do texto.

**## Como fazer:**

Extrair substrings em C# é fácil. Você só precisa usar o método `Substring()`. Veja estes exemplos:

```C#
string frase = "Amo programação em C#";
string subfrase = frase.Substring(4, 12);
Console.WriteLine(subfrase);
```
A saída será:

```C# 
programação em
```

O método `Substring(4, 12)` está a extrair uma substring que começa do 5º caractere (índice 4) e tem um comprimento de 12 caracteres.

**## Mergulho profundo:**

Historicamente, a extração de substrings está presente em praticamente todas as linguagens de programação, de Assembly a C#. Todavia, em C#, é abstraído de forma que torna mais fácil para os programadores.

Alternativas ao método `Substring()` são os métodos `Remove()` e `Split()`. O `Remove()` corta a string a partir de um índice específico enquanto o `Split()` divide a string em substrings baseado em caracteres específicos.

As substrings no C# são imutáveis, uma vez que strings em C# também são imutáveis. Isto significa que quando você cria uma substring, na realidade você está criando uma nova string.

**## Veja também:**

Você pode mergulhar mais fundo na extração de substrings com estes recursos:

1.   [Documentação Microsoft do método Substring.](https://docs.microsoft.com/pt-br/dotnet/api/system.string.substring?view=net-5.0)
2.   [Tutorial em vídeo sobre a extração de substrings em C#.](https://www.youtube.com/watch?v=TQxGgZ-5V0c)
3.   [Artigo detalhado em português sobre manipulação de textos em C#.](https://www.devmedia.com.br/trabalhando-com-strings-em-c/37982)