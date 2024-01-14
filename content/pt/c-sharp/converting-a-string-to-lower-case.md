---
title:                "C#: Convertendo uma string para minúsculas"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por Que
Converter uma string para minúsculo é uma tarefa comum na programação. Isso pode ser necessário para comparar strings de forma mais precisa ou para exibir texto em um formato uniforme. Neste artigo, mostraremos como realizar essa conversão em C#.

## Como Fazer
Para converter uma string para minúsculo em C#, podemos utilizar o método `ToLower()` da classe `string`. Veja um exemplo abaixo:

```C#
string texto = "Texto em MAIÚSCULO";
string textoMin = texto.ToLower();
Console.WriteLine(textoMin);
```

O resultado deste código será "texto em maiúsculo", como esperado. Agora, vamos criar um método que recebe uma string como parâmetro e a converte para minúsculo:

```C#
static string ConverterParaMinusculo(string texto)
{
    return texto.ToLower();
}

string texto = "OUTRO TEXTO EM MAIÚSCULO";
string textoMin = ConverterParaMinusculo(texto);
Console.WriteLine(textoMin);
```

Ao executar esse código, o resultado será novamente "texto em maiúsculo". Isso mostra que podemos utilizar o método `ToLower()` dentro de outros métodos ou funções.

## Mergulho Profundo
Na verdade, o método `ToLower()` utiliza a cultura atual para realizar a conversão. Isso significa que, em diferentes culturas, o mesmo caractere pode ter diferentes representações em minúsculo.

Por exemplo, em português, a letra "Ç" deve ser convertida para "ç" quando transformada em minúsculo. Mas em outras culturas, essa letra pode ser simplesmente removida ou substituída por outro caractere.

Para garantir que a conversão para minúsculo seja feita de acordo com a cultura esperada, podemos utilizar o método `ToLowerInvariant()`, que ignora as variações de cultura e sempre realiza a conversão da mesma maneira.

```C#
string texto = "LUCAS É COM Ç";
string textoMin = texto.ToLowerInvariant();
Console.WriteLine(textoMin);
```

O resultado, neste caso, será "lucas é com ç". É importante lembrar que, ao utilizar métodos de conversão, é necessário ter cuidado com a cultura utilizada e como isso pode afetar o resultado final.

## Veja Também
- [Documentação oficial do método `ToLower()`](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolower)
- [Cultura em C# (em inglês)](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/culture)
- [Formatando strings em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/formatting-strings)

Esperamos que esse artigo tenha ajudado você a entender melhor como converter strings para minúsculo em C#. Se tiver alguma dúvida ou sugestão, deixe um comentário abaixo. Obrigado por ler!