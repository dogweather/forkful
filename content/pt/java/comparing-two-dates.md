---
title:                "Comparando duas datas"
html_title:           "Java: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

---
## Por que comparar duas datas?

Comparar datas é uma tarefa muito comum na programação, especialmente quando lidamos com dados relacionados a tempo. É importante entender como comparar duas datas em Java para garantir a precisão e consistência dos resultados.

## Como comparar duas datas em Java

A comparação de datas em Java pode ser feita de várias maneiras, dependendo das necessidades do seu código. Aqui estão algumas das formas mais comuns de se comparar duas datas em Java:

### Comparando utilizando as classes Date e Calendar

Uma forma simples de se comparar datas é utilizando as classes Date e Calendar. Primeiro, criamos duas instâncias dessas classes com as datas que desejamos comparar. Então, podemos utilizar o método `compareTo()` para comparar as duas datas.

```Java
// Criando as datas
Date date1 = new Date();
Date date2 = new Date();

// Comparando utilizando o método compareTo()
if (date1.compareTo(date2) > 0) {
    System.out.println("A primeira data é posterior à segunda data.");
} else if (date1.compareTo(date2) < 0) {
    System.out.println("A primeira data é anterior à segunda data.");
} else {
    System.out.println("As duas datas são iguais.");
}
```

Esse método retorna um valor inteiro, sendo `1` caso a primeira data seja posterior à segunda, `-1` caso a primeira data seja anterior à segunda, e `0` caso as duas datas sejam iguais. Observação importante: a classe Date é considerada obsoleta em versões mais recentes do Java, sendo recomendado utilizar as classes do pacote `java.time` (veremos mais sobre elas adiante).

### Comparando utilizando os métodos before() e after()

Outra opção é utilizar os métodos `before()` e `after()`, presentes na classe Date. Esses métodos retornam `true` caso a primeira data seja, respectivamente, anterior ou posterior à segunda data.

```Java
// Criando as datas
Date date1 = new Date(1995, 3, 22); // 22 de março de 1995
Date date2 = new Date(1997, 8, 12); // 12 de agosto de 1997

// Verificando se a segunda data é posterior à primeira
if (date2.after(date1)) {
    System.out.println("A segunda data é posterior à primeira.");
}

// Verificando se a primeira data é anterior à segunda
if (date1.before(date2)) {
    System.out.println("A primeira data é anterior à segunda.");
}
```

### Comparando utilizando as classes LocalDate, LocalDateTime e LocalTime

Java 8 trouxe uma série de melhorias no tratamento de datas e tempos. Com isso, surgiram as classes `LocalDate`, `LocalDateTime` e `LocalTime`, presentes no pacote `java.time`. Utilizando essas classes, podemos comparar datas de forma mais intuitiva.

```Java
// Criando as datas
LocalDate date1 = LocalDate.of(2021, 1, 1); // 1 de janeiro de 2021
LocalDate date2 = LocalDate.now(); // Data atual

// Utilizando o método isAfter()
if (date2.isAfter(date1)) {
    System.out.println("A data atual é posterior ao dia 1 de janeiro de 2021");
}

// Utilizando o método isBefore()
if (date1.isBefore(date2)) {
    System.out.println("O dia 1 de janeiro de 2021 é anterior à data atual");
}
```

Também é possível utilizar os métodos `compareTo()`, `isEqual()` e `isBefore()` das classes `LocalDateTime` e `LocalTime` para comparar datas e tempos de forma mais granular.

## Aprofundando-se na comparação de datas

A comparação de datas pode ser mais complexa dependendo dos requisitos do seu código. Por exemplo, pode ser necessário considerar o fuso horário, o formato da data, diferenças em horários de verão, entre outros fatores. Para esses casos, é recomendado utilizar bibliotecas externas, como o Joda-Time ou o ThreeTen-Extra, que oferecem métodos mais robustos para a comparação de datas.

É importante também lembrar das diferenças entre as classes Date e Calendar e as classes LocalDate, LocalDateTime e LocalTime. As primeiras são mutáveis, ou seja, os valores das datas e tempos podem ser alterados após a sua criação, o que pode causar resultados inesperados na