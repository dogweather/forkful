---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenação de Strings em Java: O Que é e Como Fazer 

## O Que & Por Quê?
Concatenação de strings significa juntar duas ou mais strings. Programadores fazem isso para formar frases completas ou para formatar mensagens de saída.

## Como Fazer:
Em Java, você pode concatenar strings de diferentes maneiras. Aqui estão alguns exemplos.

```Java
// Usando o operador '+'
String saudacao = "Olá ";
String nome = "João";
String mensagem = saudacao + nome;
System.out.println(mensagem);  // Output: Olá João

// Usando o método 'concat()'
String mensagem2 = saudacao.concat(nome);
System.out.println(mensagem2);  // Output: Olá João

// Usando a classe 'StringBuilder'
StringBuilder sb = new StringBuilder(saudacao);
sb.append(nome);
String mensagem3 = sb.toString();
System.out.println(mensagem3);  // Output: Olá João
```

## Mergulho Profundo
Concatenar strings em Java tem uma história interessante.

1. Contexto Histórico: No início do Java, o operador '+' era a única maneira de concatenar strings. Mas o uso excessivo poderia levar a problemas de desempenho. Por isso, o método 'concat()' e a classe 'StringBuilder' foram introduzidos.

2. Alternativas: Além das maneiras acima, a classe 'StringBuffer' também pode ser usada para concatenar strings. É quase idêntica ao 'StringBuilder', mas é thread-safe.

3. Detalhes de Implementação: Quando você concatena strings usando o operador '+', o Java na verdade usa a classe 'StringBuilder' nos bastidores para otimizar o desempenho.

## Veja Também
Para mais informações sobre a concatenação de strings e outras operações de string em Java, confira os seguintes links:
* [Java String Documentation](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
* [StringBuilder vs StringBuffer](https://www.geeksforgeeks.org/string-vs-stringbuilder-vs-stringbuffer-in-java/)