---
title:                "Java: Extraindo Substrings"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que
Extrair substrings é uma habilidade importante para qualquer programador Java. Isso permite que você manipule e trabalhe com strings de forma eficiente, economizando tempo e esforço. Além disso, aprender a extrair substrings é uma etapa fundamental para compreender conceitos mais avançados de programação.

## Como Fazer
```Java
// Código de exemplo para extrair uma substring de uma string existente
String string = "Extrair substrings é uma habilidade importante";
String substring = string.substring(8, 18); // Retorna "substrings"
System.out.println(substring); // Saída: substrings
```

Neste exemplo, nós usamos o método `substring()` para extrair a palavra "substrings" da string original. O primeiro parâmetro indica o índice inicial da substring e o segundo parâmetro indica o índice final (exclusivo). Em outras palavras, a substring será extraída de um trecho da string original que vai do índice 8 ao 17 (pois o último índice é exclusivo).

Você também pode usar o método `substring()` para extrair uma substring de uma posição específica até o final da string, apenas fornecendo o índice inicial como parâmetro.

```Java
// Código de exemplo para extrair uma substring do início até o fim da string
String string = "Extrair substrings é uma habilidade importante";
String substring = string.substring(8); // Retorna "substrings é uma habilidade importante"
System.out.println(substring); // Saída: substrings é uma habilidade importante
```

Agora que você já sabe como extrair substrings de uma string, vamos explorar um pouco mais sobre esse assunto.

## Deep Dive
Existem duas maneiras principais de extrair substrings em Java: usando o método `substring()` ou usando a classe `StringTokenizer`.

O método `substring()` é uma opção mais simples, mas limitada. Ele só pode ser usado para extrair substrings com base na posição dos caracteres, não é possível extrair com base em conteúdo (por exemplo, extrair uma substring que contenha a palavra "importante").

Por outro lado, a classe `StringTokenizer` permite uma maior flexibilidade ao extrair substrings com base no conteúdo. Isso é especialmente útil quando se tem uma string com um formato específico, como uma data ou número.

## Veja também
- [Documentação oficial do método substring() em Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Documentação oficial da classe StringTokenizer em Java](https://docs.oracle.com/javase/8/docs/api/java/util/StringTokenizer.html)