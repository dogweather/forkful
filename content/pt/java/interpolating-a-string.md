---
title:                "Interpolando uma string"
aliases:
- pt/java/interpolating-a-string.md
date:                  2024-01-20T17:51:28.010121-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Interpolação de strings é o processo de inserir valores de variáveis dentro de uma string. Programadores utilizam isso para facilitar a construção de mensagens dinâmicas e a concatenação de informações sem a necessidade de operadores adicionais.

## Como Fazer:
```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String nome = "João";
        int idade = 25;

        // Antes do Java 15, usávamos String.format
        String mensagem = String.format("Olá, %s! Você tem %d anos.", nome, idade);
        System.out.println(mensagem);

        // Com o Java 15 e posteriores, podemos usar text blocks e o método formatted
        String novaMensagem = """
            Olá, %s! Você tem %d anos.
            """.formatted(nome, idade);
        System.out.println(novaMensagem);
    }
}
```
Saída:
```
Olá, João! Você tem 25 anos.
Olá, João! Você tem 25 anos.
```

## Aprofundamento
Antigamente, a interpolação de strings em Java era feita primariamente através de concatenação direta com o operador `+` ou com o método `String.format()`. Ambos eram pouco práticos para strings complexas. No Java 15, a JEP 378 introduziu os text blocks, permitindo uma melhor visualização e manutenção do código, ainda com o uso do `String.format()` para a interpolação.

Alternativas incluem usar `StringBuilder` ou `StringBuffer` para construções mais complexas de strings, quando a performance é crucial. No entanto, estes métodos requerem uma sintaxe mais verbosa e podem tornar o código mais difícil de ler.

A implementação da interpolação de strings permite inserir variáveis e expressões diretamente no texto, simplificando a leitura e escrita de código. Enquanto isso ainda não é possível de forma nativa no Java, cenários similares podem ser alcançados com text blocks e o método `formatted()`, como mostrado acima.

## Veja Também
- [Documentação Oficial do Java - String.format()](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))
- [JEP 378: Text Blocks](https://openjdk.java.net/jeps/378)
