---
title:                "Java: Encontrando o comprimento de uma string."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação Java, pois fornece informações valiosas sobre os dados que estamos manipulando. Saber o tamanho da string pode nos ajudar a validar entradas do usuário, formatar saídas de acordo com um limite de caracteres e realizar várias operações de manipulação de strings de maneira eficiente.

##Como Fazer

Para encontrar o comprimento de uma string em Java, podemos usar o método length() da classe String. Vamos ver um exemplo simples:

`` `Java
String nome = "Maria";
int tamanho = nome.length ();

System.out.println ("O nome tem" + tamanho + "caracteres");
`` `

A saída deste código será "O nome tem 5 caracteres". Podemos ver que o método length() retorna o número de caracteres na string, incluindo espaços em branco.

Também podemos usar o método length() para verificar se uma string está vazia. Por exemplo:

`` `Java
String nome = "";
int tamanho = nome.length ();

if (tamanho == 0) {
    System.out.println ("A string está vazia");
}
`` `

A saída será "A string está vazia", pois o método length() retorna 0 para uma string vazia.

##Mergulho Profundo

Agora que sabemos como usar o método length() para encontrar o comprimento de uma string, é importante entender como ele funciona por baixo dos panos. O método length() retorna o valor do atributo "count" na classe String, que é atualizado sempre que a string é alterada.

É importante lembrar que o método length() só pode ser usado em objetos String e não pode ser chamado em matrizes. Além disso, ele conta o número de caracteres Unicode na string, o que pode ser diferente do número de caracteres visíveis.

##Veja Também
- Documentação do método length(): https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--
- Tutorial de manipulação de strings em Java: https://www.geeksforgeeks.org/strings-in-java/
- Diferenças entre caracteres e bytes em Java: https://www.baeldung.com/java-char-byte-conversions