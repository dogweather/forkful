---
title:                "Concatenando strings"
html_title:           "Java: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e Porque?

A concatenação de strings é basicamente a combinação de duas ou mais strings em uma única string. Programadores frequentemente fazem isso para criar mensagens personalizadas para usuários, gerar saídas de dados ou construir URLs.

## Como fazer:

```java
// Exemplo 1: Concatenando duas strings
String saudacao = "Olá";
String nome = "João";
String mensagem = saudacao + " " + nome; // Resultado: "Olá João"

// Exemplo 2: Adicionando números a uma string
String sentimento = "Eu tenho";
int quantidade = 3;
String acao = "gatos";
String descricao = sentimento + " " + quantidade + " " + acao; // Resultado: "Eu tenho 3 gatos"

// Exemplo 3: Concatenando com método concat()
String palavra1 = "Hello";
String palavra2 = "world";
String frase = palavra1.concat(" ").concat(palavra2); // Resultado: "Hello world"

// Exemplo 4: Usando o método join()
String[] cores = {"vermelho", "amarelo", "azul"};
String coresJuntas = String.join(", ", cores); // Resultado: "vermelho, amarelo, azul"

System.out.println(mensagem);
System.out.println(descricao);
System.out.println(frase);
System.out.println(coresJuntas);
```

Resultado:
```
Olá João
Eu tenho 3 gatos
Hello world
vermelho, amarelo, azul
```

## Mergulho Profundo:

A concatenação de strings existe desde os primeiros dias da programação de computadores. Antigamente, era uma tarefa mais complexa e demorada, mas com o avanço da tecnologia e o surgimento de linguagens como Java, agora é mais fácil de realizar.

Uma alternativa à concatenação de strings é o uso de StringBuilder, que permite a manipulação de strings sem criar um novo objeto a cada alteração.

Alguns detalhes sobre concatenação de strings em Java:
- O operador "+" é usado para concatenar strings em Java.
- Usar o método concat() é mais eficiente do que o operador "+".
- Concatenação de strings pode ser usada com qualquer tipo de dado, não apenas com strings.

## Veja Também:

- [Documentação oficial do Java sobre strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Tutorial sobre concatenação de strings em Java](https://www.w3schools.com/java/java_strings_concat.asp)
- [Comparação entre concat() e operador "+" em Java](https://www.developer.com/java/data/why-you-should-use-stringbuilders-in-java.html)