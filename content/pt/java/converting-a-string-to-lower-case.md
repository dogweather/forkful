---
title:                "Java: Convertendo uma string para minúsculas"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
A conversão de uma string para letras minúsculas é uma tarefa comum na programação, especialmente quando lidamos com dados de usuário ou manipulamos strings em geral. Ao converter uma string para letras minúsculas, podemos garantir que a comparação de strings seja feita de forma consistente e precisa.

## Como Fazer
```Java
// Criando uma string com letras maiúsculas
String exemplo = "CONVERTE-ME PARA LETRAS MINÚSCULAS"; 
// Utilizando o método toLowerCase() para converter a string para letras minúsculas
String resultado = exemplo.toLowerCase();
// Imprimindo o resultado
System.out.println(resultado);

// Output: converte-me para letras minúsculas
```

Ao utilizar o método `toLowerCase()`, garantimos que todas as letras da string sejam convertidas para minúsculas, incluindo acentos e caracteres especiais. Além disso, é importante lembrar que strings são imutáveis em Java, então o método `toLowerCase()` irá criar uma nova string com as modificações realizadas, preservando a string original.

## Mergulho Profundo
O método `toLowerCase()` é uma função da classe `String` do Java, e é responsável por converter todas as letras maiúsculas da string para letras minúsculas. Ele é baseado na tabela ASCII, que é um sistema de mapeamento de caracteres usado em computadores.

Ao converter uma string para letras minúsculas, o Java utiliza esse mapeamento para trocar os valores numéricos de cada caractere, mudando-os para o equivalente em letras minúsculas. Por exemplo, a letra "A" possui um valor numérico na tabela ASCII de 65, enquanto a letra "a" possui o valor 97. O método `toLowerCase()` irá trocar esse valor de 65 para 97, convertendo a letra maiúscula "A" para minúscula "a".

Além disso, é importante lembrar que a conversão de letras maiúsculas para minúsculas pode ser dependente da localização (locale) em que o programa está sendo executado. Isso significa que caracteres com acentos, como "Á" ou "Â", podem ser convertidos de forma diferente dependendo da região. Portanto, é sempre importante utilizar a conversão de strings de forma consciente e considerar possíveis diferenças na localização.

## Veja também
- [Documentação oficial do método toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Tabela ASCII](https://www.ascii-code.com/)
- [Java Tutorial: Strings and Text](https://docs.oracle.com/javase/tutorial/i18n/text/string.html)