---
title:    "Java: Encontrando o comprimento de uma string"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação. Saber como fazer isso pode ajudar nas operações de manipulação de strings e outras tarefas de programação.

## Como fazer

Encontrar o comprimento de uma string em Java é simples usando o método `length()`. Veja o exemplo abaixo:

```Java
String minhaString = "Olá, mundo!";
int comprimento = minhaString.length();
System.out.println(comprimento);
```

A saída do código acima será "12", pois existem 12 caracteres na string "Olá, mundo!". Outra forma de encontrar o comprimento de uma string é usando a propriedade `length()` na classe `String`. Veja o exemplo:

```Java
String minhaString = "Estou aprendendo Java!";
int comprimento = minhaString.length();
System.out.println("O comprimento da string é: " + comprimento);
```

A saída do segundo exemplo será "24", pois a string contém 24 caracteres. É importante lembrar que espaços em branco também contam como caracteres na string.

## Mergulho profundo

Ao lidar com strings em Java, é importante entender que o método `length()` retorna o número de caracteres, e não o número de bytes. Isso pode ser um fator importante ao trabalhar com diferentes idiomas e caracteres especiais.

Além disso, o método `length()` só retorna inteiros positivos. Se quiser obter o tamanho de uma string vazia, o valor retornado será 0, e para uma string nula, irá gerar um erro.

A classe `String` também possui o método `isEmpty()`, que pode ser utilizado para verificar se uma string está vazia antes de chamar o método `length()`, evitando assim possíveis erros.

## Veja também

- Documentação oficial do método `length()` na classe `String`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--
- Artigo sobre strings em Java: https://www.devmedia.com.br/trabalhando-com-strings-em-java/29703 
- Vídeo tutorial sobre o método `length()`: https://www.youtube.com/watch?v=-npS28pBJDg