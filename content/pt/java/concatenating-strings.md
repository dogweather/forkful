---
title:    "Java: Juntando strings"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, em programação, é necessário juntar duas ou mais strings para criar uma nova. O processo de concatenação de strings é essencial para manipular e formatar dados de maneira eficiente. Neste blog post, vamos discutir por que a concatenação de strings é importante e como fazer isso em Java.

## Como Fazer

Em Java, existem várias maneiras de concatenar strings. A forma mais simples é usando o operador "+" para juntar duas ou mais strings.

```Java
String nome = "Carlos";
String sobrenome = "Silva";
String nomeCompleto = nome + " " + sobrenome;
System.out.println(nomeCompleto);
```

**Output:**
```
Carlos Silva
```

Outra forma de concatenar strings é usando o método `concat()` da classe `String`. Este método recebe uma string como parâmetro e a adiciona à string existente.

```Java
String linguagem = "Java";
String frase = linguagem.concat(" é uma linguagem de programação.").concat(" Muito poderosa.");
System.out.println(frase);
```

**Output:**
```
Java é uma linguagem de programação. Muito poderosa.
```

Também é possível concatenar strings usando o método `StringBuilder` ou `StringBuffer`. Essas classes são mais eficientes para manipular grandes quantidades de strings, pois permitem a modificação direta do objeto, em vez de criar um novo a cada concatenação.

```Java
StringBuilder nome = new StringBuilder("Maria");
nome.append(" ");
nome.append("Silva");
System.out.println(nome.toString());
```

**Output:**
```
Maria Silva
```

## Deep Dive

No Java, as strings são imutáveis, o que significa que depois de criadas, não podem ser alteradas. Ao concatenar strings, na verdade, estamos criando novos objetos no processo. Por isso, é importante ter cuidado ao concatenar muitas strings, pois pode levar a uma sobrecarga de memória e afetar o desempenho do programa.

Além disso, deve-se ter atenção ao usar o operador "+", pois ele pode ser menos eficiente do que outros métodos de concatenação quando usado em loops, por exemplo. Nesses casos, é recomendado o uso de `StringBuilder` ou `StringBuffer` para uma melhor performance.

## Veja Também

- [Documentação oficial do Java sobre strings](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial sobre concatenação de strings em Java](https://www.w3schools.com/java/java_strings_concat.asp)
- [Diferença entre StringBuilder e StringBuffer](https://www.baeldung.com/java-stringbuilder-stringbuffer)