---
title:    "C++: Extraindo subcadeias de caracteres."
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que extrair substrings em C++?

Extrair substrings ou trechos de texto de uma string maior é uma tarefa muito comum em programação. Isso pode ser útil em várias situações, como manipulação de dados, análise de texto ou formatação de saída.

## Como fazer em C++

Há diversas maneiras de extrair substrings em C++, mas a forma mais comum é usando a função `substring()` da classe `string`. Por exemplo, suponha que queremos extrair os três primeiros caracteres de uma string:

```C++
// Criando uma string
string texto = "Extraindo substrings em C++";

// Extraindo os três primeiros caracteres
string sub = texto.substr(0, 3);

// Output: Ext
cout << sub << endl;
```

Neste exemplo, usamos a função `substr()` passando dois argumentos: a posição inicial e o número de caracteres que queremos extrair. Assim, a substring `Ext` é extraída da string original e armazenada na variável `sub`.

Também é possível extrair uma substring a partir de uma posição específica até o final da string original, simplesmente omitindo o segundo argumento:

```C++
string texto = "Extraindo substrings em C++";

// Extraindo a partir do sexto caractere até o final
string sub = texto.substr(5);

// Output: indo substrings em C++
cout << sub << endl;
```

## Aprofundando

Quando usamos a função `substr()` em C++, é importante lembrar que a posição inicial começa em 0, ou seja, o primeiro caractere tem posição 0, o segundo tem posição 1 e assim por diante. Além disso, a função também permite extrair substrings de tamanhos diferentes, como por exemplo:

```C++
string texto = "Extraindo substrings em C++";

// Extraindo os cinco primeiros caracteres
string sub = texto.substr(0, 5);

// Output: Extra
cout << sub << endl;
```

Além disso, é importante lembrar que a função `substr()` não altera a string original, apenas retorna uma nova string com a substring desejada. Caso seja necessário alterar a string original, é possível usar a função `replace()` da classe `string`.

## Veja também

- [Documentação da função `substr()` em C++](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorial sobre strings em C++](https://www.geeksforgeeks.org/string-class-in-cpp/)
- [Exemplos e exercícios práticos de substrings em C++](https://codeforwin.org/2018/08/cpp-program-to-extract-substring-from-a-string.html)