---
title:                "C#: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Converter uma string para letras minúsculas é uma tarefa comum em programação, especialmente ao manipular texto e realizar comparações. Ao fazer isso, você garante que as letras maiúsculas não se tornem um obstáculo para o seu código.

## Como fazer

Aqui vai um exemplo simples de como converter uma string para letras minúsculas em C#:

```C#
string texto = "EStE é UM teXto eMp MaiúSCuLAS";
Console.WriteLine(texto.ToLower());
```

Saída:
```
este é um texto em maiúsculas
```

O método "ToLower()" é usado para converter todas as letras maiúsculas em minúsculas. É importante notar que o original não é alterado, mas sim uma cópia é criada com as letras minúsculas.

Você também pode usar este método em variáveis de tipo "char" para converter caracteres individuais. Além disso, você pode usar "ToUpper()" para fazer o inverso - converter todas as letras minúsculas em maiúsculas.

## Mergulho profundo

Vamos entender um pouco mais sobre o processo de conversão de string para letras minúsculas. Quando usamos o método "ToLower()", ele basicamente percorre cada caractere da string e, caso encontre uma letra maiúscula, converte-a em minúscula.

No entanto, isso pode ser um problema para idiomas com letras acentuadas, como o português. Por exemplo, a letra "é" quando convertida para maiúscula resulta em "É", que é um caractere completamente diferente. Nesses casos, é importante usar o método "ToLowerInvariant()", que garante que as regras de capitalização sejam aplicadas de forma consistente, independentemente do idioma.

Outro detalhe importante é que o método "ToLower()" usa as configurações de cultura do sistema operacional, o que pode variar de acordo com o dispositivo que está executando o código. Se você deseja garantir que a conversão seja feita de forma consistente, é recomendado especificar a cultura desejada ao usar o método.

## Veja também

- [Método ToLower() da documentação oficial do C#](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolower)
- [Cultura no C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/using-locale-specific-data)
- [Como lidar com letras acentuadas em C#](https://codewithshadman.com/c-sharp-accented-characters/)