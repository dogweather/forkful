---
title:                "Usando expressões regulares"
html_title:           "Java: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que é e porquê?
Expressões regulares são padrões de texto usados para procurar e manipular strings em um programa Java. Elas são úteis para realizar tarefas como validação de entrada de usuário, busca e substituição de texto e filtragem de dados. Programadores usam expressões regulares porque elas permitem a criação de código conciso e eficiente para lidar com tarefas de processamento de texto.

## Como fazer:
Para usar expressões regulares em um programa Java, é necessário importar a classe "java.util.regex". Depois disso, basta seguir a sintaxe abaixo para realizar as operações desejadas:

```Java
//Validação de entrada de usuário
Pattern pattern = Pattern.compile("[a-z]+"); //cria um padrão que aceita apenas letras minúsculas
Matcher matcher = pattern.matcher(input); //aplica o padrão na string de entrada
if (matcher.matches()) { //verifica se a string de entrada corresponde ao padrão
  System.out.println("Entrada válida!");
} else {
  System.out.println("Entrada inválida!");
}

//Busca e substituição de texto
String texto = "Olá mundo!";
String novoTexto = texto.replaceAll("mundo", "programador"); //substitui "mundo" por "programador"
System.out.println(novoTexto); //imprime "Olá programador!"

//Filtragem de dados
String[] palavras = {"casa", "carro", "avião"};
for (String palavra : palavras) {
  if (palavra.matches(".*a.*")) { //verifica se a string contém a letra "a"
    System.out.println(palavra); //imprime "casa", "carro" e "avião"
  }
}
```

## Mergulho Profundo:
As expressões regulares foram inventadas por Stephen Kleene na década de 1950 na área da teoria da computação. Elas foram adotadas pela linguagem Perl na década de 1980 e posteriormente implementadas em outras linguagens de programação, incluindo Java. Alternativas para expressões regulares em Java incluem o uso de bibliotecas de terceiros, como o Apache Commons Text ou o Google Guava, que oferecem funcionalidades adicionais.

As expressões regulares em Java são baseadas na API java.util.regex, que possui duas classes principais: Pattern e Matcher. A classe Pattern representa um padrão de expressão regular e a classe Matcher é usada para aplicar esse padrão a uma string e realizar operações como busca, substituição e validação.

## Veja também:
- [Documentação oficial do Java sobre expressões regulares](https://docs.oracle.com/javase/10/docs/api/java/util/regex/package-summary.html)
- [Tutorial sobre expressões regulares em Java da Oracle](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)