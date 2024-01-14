---
title:                "C#: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair Substrings?

Extrair substrings de uma string em C# pode ser útil em muitos casos! Isso permite que você obtenha apenas uma parte específica de uma string, o que pode ser útil ao manipular dados ou formatar saídas de texto.

## Como fazer:

Para extrair uma substring em C#, podemos usar o método `Substring()` da classe `String`. Este método requer dois parâmetros: o índice inicial e o comprimento da substring.

```C#
// Exemplo de uma string
string texto = "Olá, meu nome é Maria!";

// Extraindo a substring 'Maria'
string nome = texto.Substring(16, 5);

Console.WriteLine(nome); // Output: Maria
```

Neste exemplo, passamos o índice inicial de 16, que é a posição do primeiro caractere da palavra Maria na string. E então, passamos o comprimento da substring desejada, que é 5 (o número de caracteres em 'Maria').

Também podemos usar o método `Substring()` para obter uma substring do final de uma string. Para isso, basta passar um valor negativo como índice inicial.

```C#
// Exemplo de uma string
string email = "usuario@exemplo.com";

// Extraindo o domínio do email
string dominio = email.Substring(email.IndexOf("@") + 1);

Console.WriteLine(dominio); // Output: exemplo.com
```

Neste exemplo, usamos o método `IndexOf()` para obter a posição do caractere '@' na string e somamos 1 (para não incluir o '@' na substring resultante). Então, passamos esse valor negativo como índice inicial para extrair a substring do domínio do email.

## Mais Informações:

Agora que você sabe como extrair substrings em C#, é importante ter em mente que o índice inicial deve estar dentro dos limites da string. Caso contrário, será lançada uma exceção `ArgumentOutOfRangeException`.

Além disso, se o comprimento da substring resultante for maior que o número de caracteres restantes na string, apenas os caracteres restantes serão extraídos.

## Veja Também:

Aqui estão alguns recursos úteis relacionados à extração de substrings em C#:

- Documentação da Microsoft sobre o método `Substring`: https://docs.microsoft.com/pt-br/dotnet/api/system.string.substring
- Tutorial sobre manipulação de strings em C#: https://www.tutorialspoint.com/csharp/csharp_strings.htm
- Exemplo de projeto no GitHub demonstrando o uso de `Substring`: https://github.com/RodrigoDomingues/AlterandoTextoEmC-
- Comunidade de desenvolvedores C# no Brasil: https://csharpbrasil.com.br/