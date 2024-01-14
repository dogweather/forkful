---
title:    "Java: Unindo strings"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma operação fundamental em Java e é frequentemente usada em programação para combinar duas ou mais strings em uma única string. Isso pode ser útil ao mostrar informações de saída para o usuário, como em um log de atividades ou em uma interface gráfica.

## Como Fazer

Para concatenar strings em Java, podemos usar o operador de adição (+) ou o método concat () da classe String. Vamos ver um exemplo de cada um:

```
String str1 = "Java";
String str2 = "é incrível!";
String result = str1 + " " + str2;
System.out.println(result);
```

Este código irá produzir a saída "Java é incrível!".

Também podemos usar o método concat () da seguinte maneira:

```
String str1 = "Java";
String str2 = "é incrível!";
String result = str1.concat(" ").concat(str2);
System.out.println(result);
```

Este código produz a mesma saída que o exemplo anterior.

## Profundidade

A operação de concatenação em Java é mais do que apenas combinar strings. Quando usamos o operador de adição (+) para concatenar strings, o compilador Java converte o código em um método StringBuilder () internamente, o que é mais eficiente do que usar o método concat (). Além disso, ao usar o operador de adição com strings muito longas, podemos obter um melhor desempenho do que quando usamos o método concat ().

No entanto, é importante lembrar que o Java não permite a concatenação de strings com outros tipos de dados, como inteiros ou floats. Nesses casos, é necessário converter os valores em string antes de concatená-los.

## Veja também

Se você quiser saber mais sobre concatenação de strings em Java, aqui estão alguns links úteis:

- [Documentação do Java sobre concatenação de strings](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Tutorial básico sobre concatenação de strings em Java](https://www.baeldung.com/java-string-concatenation)
- [Perguntas frequentes sobre concatenação de strings em Java](https://www.geeksforgeeks.org/stringconcat-method-in-java-with-examples/)