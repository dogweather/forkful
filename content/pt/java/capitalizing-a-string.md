---
title:    "Java: Capitalizando uma string"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Java?

A capitalização de uma string é um processo fundamental para a qualidade e legibilidade do código em Java. Ao capitalizar uma string, garantimos que as informações apresentadas ao usuário estejam uniformes e corretas.

## Como capitalizar uma string em Java

Para capitalizar uma string em Java, temos duas opções: usando o método `toUpperCase()` ou aproveitando a biblioteca `StringUtils` do Apache Commons Lang.

```Java
// Exemplo usando o método toUpperCase()

String nome = "carlos";
String nomeCapitalizado = nome.toUpperCase();
System.out.println(nomeCapitalizado); // Saída: CARLOS

// Exemplo usando a biblioteca StringUtils

String nome = "ana";
String nomeCapitalizado = StringUtils.capitalize(nome);
System.out.println(nomeCapitalizado); // Saída: Ana
```

## Profundidade no Capitalization

Capitalizar uma string requer um conhecimento mais profundo sobre a forma como as strings são armazenadas e manipuladas em memória no Java. Isso nos permite entender o impacto de diferentes técnicas de capitalização na performance do nosso código.

Além disso, é importante estar ciente de que diferentes idiomas possuem suas próprias regras de capitalização, e é fundamental respeitar essas regras para garantir a corretude e a fluência do texto apresentado ao usuário.

## Veja também

- [Documentação oficial da classe String em Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Documentação da biblioteca Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
- [Dicas para melhorar a performance em Java](https://www.devmedia.com.br/dez-dicas-para-melhorar-a-performance-em-java/25642)