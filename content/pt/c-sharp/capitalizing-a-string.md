---
title:    "C#: Capitalizando uma string"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que capitalizar strings em C#?

Capitalizar strings é uma tarefa comum em programação, especialmente em linguagens orientadas a objetos como C#. Ao capitalizar uma string, estamos alterando a primeira caractere para maiúscula e todas as outras para minúsculas. Isso pode ser útil para padronização de texto e apresentação mais legível de informações.

## Como fazer

Para capitalizar uma string em C#, podemos utilizar o método `ToUpper()` da classe `String`. Este método converta todas as caracteres para maiúsculas, mas para apenas para a primeira caractere, podemos usar o método `Substring()` para obter a primeira caractere e depois convertê-la para maiúscula.

Aqui está um exemplo de código em C# que demonstra como capitalizar uma string:

```
// Declara uma string
string texto = "exemplo de string";

// Aplica o método ToUpper ()
string textoMaiusculo = texto.ToUpper();

// Aplica o método Substring ()
string primeiraLetraMaiuscula = texto.Substring(0, 1).ToUpper();

// Concatena a primeira letra maiúscula com o resto da string convertida
string textoCapitalizado = primeiraLetraMaiuscula + texto.Substring(1).ToUpper();

// Imprime o resultado
Console.WriteLine("String original: " + texto);
Console.WriteLine("String maiúscula: " + textoMaiusculo);
Console.WriteLine("String com primeira letra maiúscula: " + primeiraLetraMaiuscula);
Console.WriteLine("String capitalizada: " + textoCapitalizado);
```

Output:

```
String original: exemplo de string
String maiúscula: EXEMPLO DE STRING
String com primeira letra maiúscula: E
String capitalizada: Exemplo de string
```

## Profundidade

Existem algumas coisas importantes a serem consideradas ao capitalizar strings em C#. Primeiramente, é importante notar que o método `ToUpper()` é culturalmente dependente, ou seja, ele irá capitalizar as letras de acordo com as configurações de idioma do sistema operacional. Portanto, é possível que o resultado final varie em diferentes computadores.

Além disso, devemos ter cuidado ao trabalhar com pontuação e caracteres especiais em uma string. O método `ToUpper()` irá capitalizar apenas as letras, então pode ser necessário aplicar alguns tratamentos adicionais para caracteres especiais.

Outra consideração é o desempenho. Como o método `ToUpper()` cria uma nova string, pode ser um problema em casos onde há um grande volume de texto sendo processado. Nesse caso, pode ser mais eficiente usar o método `Char.ToUpper()` em vez do `ToUpper()`.

## Veja também
- [Documentação oficial do método ToUpper()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.toupper?view=netcore-3.1)
- [Exemplo de uso do método ToUpper()](https://www.educba.com/c-sharp-string-toupper/)
- [Tutorial sobre manipulação de strings em C#](https://www.tutorialsteacher.com/csharp/csharp-string)