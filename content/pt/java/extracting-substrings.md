---
title:    "Java: Extraindo Substrings"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que extrair substrings pode ser útil em Java?

Extrair substrings de uma string é uma habilidade importante em Java, pois permite manipular e trabalhar com partes específicas de uma palavra, frase ou texto. Isso pode ser útil em muitos cenários de programação, desde a validação de entradas de usuário até a formatação de dados para exibição.

## Como extrair substrings em Java

Para extrair uma substring em Java, podemos usar o método `substring()` da classe `String`. Este método requer dois parâmetros: o índice inicial e o índice final da substring que desejamos extrair.

Aqui está um exemplo de código para extrair uma substring da palavra "programação":

```Java
String palavra = "programação";
String substring = palavra.substring(2, 8); // extrai a substring "ograma"
System.out.println(substring); // imprime "ograma"
```

Podemos até mesmo usar valores negativos para extrair substrings contando a partir do fim da string:

```Java
String palavra = "programação";
String substring = palavra.substring(-5, -2); // extrai a substring "gra"
System.out.println(substring); // imprime "gra"
```

É importante notar que o índice inicial é inclusive, enquanto o índice final é exclusivo. Isso significa que o caractere no índice final não será incluído na substring.

Outra opção para extrair substrings é usar o método `substring()` da classe `StringBuilder`. Este método também requer um índice inicial, mas não requer um índice final. Aqui está um exemplo:

```Java
StringBuilder builder = new StringBuilder("programação");
String substring = builder.substring(4); // extrai a substring a partir do índice 4 (inclusive)
System.out.println(substring); // imprime "ramação"
```

## Profundidade no assunto da extração de substrings

Além dos métodos mencionados acima, Java oferece uma variedade de outras opções para extrair substrings, como o método `split()` para separar strings em substrings com base em um separador específico.

Também é importante estar ciente de possíveis erros ao extrair substrings, como fornecer índices inválidos ou ultrapassar o comprimento da string. É sempre uma boa prática incluir verificações de validação antes de extrair substrings.

## Veja também

- Documentação oficial do método `substring()` da classe `String` em Java: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int
- Documentação oficial do método `substring()` da classe `StringBuilder` em Java: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html#substring-int-