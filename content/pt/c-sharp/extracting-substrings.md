---
title:                "Extraindo Substrings"
html_title:           "C#: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

Por que extrair substrings é útil?

Extrair substrings é uma técnica comum em programação e pode ser extremamente útil em diversas situações. Com ela, é possível obter apenas uma parte de uma string maior, facilitando a manipulação e análise de dados. Por exemplo, ao trabalhar com dados de usuários, podemos extrair apenas o número de telefone de uma string que contém informações diversas, tornando o processo de obtenção de informações mais eficiente e preciso.

Como extrair substrings em C#?

Para extrair substrings em C#, é preciso utilizar o método Substring da classe String. Este método recebe como parâmetros o índice inicial e o comprimento da substring desejada e retorna uma nova string contendo apenas os caracteres desejados. Veja um exemplo abaixo:

```
string texto = "Este é um texto de exemplo";
string substring = texto.Substring(8, 4);
Console.WriteLine(substring);
```

Este código irá imprimir no console a substring "texto".

Profundidade sobre a extração de substrings

É importante lembrar que a contagem de índice em C# começa em 0. Portanto, no exemplo acima, passamos o índice 8 para indicar que queremos começar a substring a partir do nono caractere do texto original. Além disso, o comprimento também deve ser especificado para indicar quantos caracteres serão incluídos na substring final.

Outra funcionalidade interessante do método Substring é a possibilidade de omitir o parâmetro de comprimento, o que resulta na extração de todos os caracteres a partir do índice inicial até o final da string. Veja outro exemplo:

```
string texto = "Este é outro exemplo de texto";
string substring = texto.Substring(8);
Console.WriteLine(substring);
```

Neste caso, o código irá imprimir no console a substring "outro exemplo de texto", pois não especificamos um comprimento e o método trata todos os caracteres após o índice inicial como parte da substring.

Em resumo, a extração de substrings é uma técnica útil para manipular e obter informações específicas de uma string maior. Com o método Substring em C#, é possível extrair facilmente partes de uma string, tornando o processo de programação mais eficiente e preciso.

Veja também:

- Documentação oficial do método Substring: https://docs.microsoft.com/pt-br/dotnet/api/system.string.substring
- Artigo sobre manipulação de strings em C#: https://www.devmedia.com.br/strings-em-c-aprenda-a-manipular-strings/28555