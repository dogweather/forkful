---
title:    "Java: Converter uma string para minúsculo"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que?

Converter uma string para letra minúscula é uma tarefa comum em programação, especialmente quando se trabalha com entradas de usuário ou manipulação de dados. Ao converter uma string para minúsculas, podemos garantir que o programa será capaz de reconhecer e comparar palavras ou caracteres da mesma forma, independentemente de estarem em letras maiúsculas ou minúsculas.

## Como Fazer

Para converter uma string para minúsculas em Java, podemos usar o método `toLowerCase()` da classe `String`. Veja o exemplo abaixo:

```Java
String nome = "Olá, Mundo!";
String nomeMinusculo = nome.toLowerCase();

System.out.println(nomeMinusculo);
```

A saída desse código será `olá, mundo!`, com todas as letras em minúsculo. Além disso, esse método também funciona para converter caracteres especiais, como a letra "Á", para a versão minúscula "á".

```Java
String palavra = "MAÇÃ";
String palavraMinusculo = palavra.toLowerCase();

System.out.println(palavraMinusculo);
```

A saída será `maçã`, com todos os caracteres em minúsculo, incluindo o "ç".

## Profundidade

Para entendermos melhor como funciona a conversão de strings para minúsculas em Java, é importante entender que essa operação é realizada com base na tabela ASCII. A tabela ASCII é um padrão de codificação de caracteres e é utilizada para representar letras, números, símbolos e caracteres especiais em uma linguagem de programação.

Em termos técnicos, quando usamos o método `toLowerCase()`, o Java utiliza o código ASCII de cada caractere para determinar se ele deve ser convertido para minúsculo ou não. Por exemplo, o código ASCII de "A" é 65 e o código de "a" é 97. Ao subtrair 32 do código ASCII de qualquer letra maiúscula, obtemos o código ASCII correspondente à mesma letra em minúsculo. É por isso que, na saída do exemplo anterior, a letra "A" foi convertida para "a" e a letra "Ç" para "ç".

## Veja Também

- [Documentação oficial do método `toLowerCase()`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [Tutorial sobre a tabela ASCII na Programação em Java](https://www.devmedia.com.br/tabela-ascii-e-a-sua-importancia-na-programacao-em-java/31668)
- [Exemplo prático de conversão para minúsculas em Java](https://www.caelum.com.br/apostila-java-orientacao-objetos/metodos-de-objetos/#19-19-exercicio-fazendo-conversoes)

Obrigado por ler este post! Esperamos que tenha sido útil para entender a conversão de strings para minúsculas em Java. Se tiver alguma dúvida ou sugestão, deixe um comentário abaixo. Até a próxima!