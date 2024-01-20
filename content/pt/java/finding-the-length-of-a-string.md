---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Encontrar o comprimento de uma string significa determinar o número de caracteres contidos naquela string. Programadores fazem isso frequentemente para validar entradas do usuário, para manipular texto, entre outros usos.

## Como Fazer:
Aqui está um exemplo de como você pode determinar o comprimento de uma string em Java:

```Java
public class Main {
    public static void main(String[] args) {
        String texto = "Olá, Mundo!";
        int comprimento = texto.length();

        System.out.println("O comprimento da string é: " + comprimento);
    }
}
```
A saída deste código será:

```
O comprimento da string é: 11
```

## Mergulho Profundo
A função `length()` tem sido um recurso comum em linguagens de programação como Java há muito tempo. É uma implementação direta de uma operação comum que você pode realizar em uma string: contar o número de caracteres.

Existem outras técnicas para encontrar o comprimento de uma string sem usar o método `length()`. Por exemplo, você pode iterar sobre a string até atingir o caractere nulo. No entanto, esse método pode ser mais suscetível a erros, portanto `length()` é a maneira recomendada.

Quanto à implementação do `length()`, em Java, a informação de comprimento é armazenada diretamente no objeto String, o que torna a chamada do método `length()` muito eficiente. Não precisamos passar por todos os caracteres a cada vez que quisermos encontrar o comprimento da string.

## Veja Também
Para saber mais sobre as strings em Java, confira os seguintes recursos:

1. [Documentação oficial do Java para a classe String](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
2. [Tutorial do Oracle sobre strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)