---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?

Converter uma string para minúscula é a prática de transformar todo o texto em letras minúsculas. Programadores fazem isso para impedir problemas de case-sensitivity (sensibilidade ao caso) e padronizar os dados de entrada.

## Como Fazer:

Em Java, a conversão de strings para minúscula é realizada pelo método `toLowerCase()`. Aqui está o código de amostra:

```java
public class Main {
    public static void main(String[] args) {
        String frase = "Hello, World!";
        String fraseMin = frase.toLowerCase();
        System.out.println(fraseMin);
    }
}
```

A saída será:

```java
"hello, world!"
```

## Mergulhando Fundo:

- **Contexto histórico**: Java é uma linguagem orientada a objetos lançada em 1995. Desde o início, a classe String tem sido um pilar fundamental. O método `toLowerCase()` está disponível desde a primeira versão.
- **Alternativas**: No Java, a conversão de string para minúscula usando o método `toLowerCase()` é a opção mais direta. Porém, lembre-se de que o comportamento deste método pode variar dependendo das configurações de localização do usuário.
- **Detalhes da implementação**: O método `toLowerCase()` funciona analisando cada caractere do texto. Se o caractere for uma letra maiúscula, ele o transforma em minúsculo. Se o caractere não for uma letra maiúscula, ele o deixa inalterado.

## Veja Também:

- [Documentação Oficial do Java - String](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [Tutorial da Oracle sobre Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)