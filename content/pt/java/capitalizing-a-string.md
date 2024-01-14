---
title:                "Java: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Por que capitalizar uma string?

Ao trabalhar com Java, às vezes é necessário alterar a capitalização de uma string, ou seja, converter todas as letras para maiúsculas ou minúsculas. Isso pode ser útil quando se trabalha com entradas de usuário ou quando se precisa comparar strings independentemente da capitalização. Neste post, vamos explorar como realizar essa tarefa de forma simples e eficiente em Java.

Como fazer:

```Java
public static void main(String[] args) {
  // Exemplo de string
  String texto = "exemplo de string";
  
  // Conversão para letras maiúsculas
  String maiusculas = texto.toUpperCase();
  System.out.println(maiusculas);
  
  // Conversão para letras minúsculas
  String minusculas = texto.toLowerCase();
  System.out.println(minusculas);
}

/* Output:
EXEMPLO DE STRING
exemplo de string
*/
```

A classe String em Java possui dois métodos principais para realizar a capitalização: `toUpperCase()` e `toLowerCase()`. Ambos os métodos retornam uma nova string, mantendo a string original intacta. O método `toUpperCase()` transforma todas as letras em maiúsculas, enquanto o `toLowerCase()` as converte para minúsculas. Esses métodos também são úteis quando se trabalha com comparação de strings, já que podem ajudar a evitar erros relacionados à capitalização.

Além disso, é possível capitalizar apenas a primeira letra de uma string utilizando o método `substring()` combinado com a função `toUpperCase()`, como mostrado no exemplo abaixo:

```Java
// Exemplo de string
String nome = "joão da silva";

// Obtendo a primeira letra maiúscula
String primeiraLetra = nome.substring(0, 1).toUpperCase();

/* Output:
J
*/
```

Deep Dive:

Embora os métodos `toUpperCase()` e `toLowerCase()` sejam simples e práticos, é importante ter em mente algumas considerações ao usar essas funções. Primeiramente, é importante mencionar que esses métodos dependem do idioma padrão do sistema operacional. Portanto, se o idioma padrão for diferente de inglês, os resultados podem diferir do esperado.

Outro ponto importante é que, ao converter uma string para letras maiúsculas ou minúsculas, os caracteres acentuados e especiais não são modificados. Por exemplo, a palavra "água" será convertida para "ÁGUA", mas os acentos permanecerão. É importante considerar essa situação ao trabalhar com caracteres especiais em diferentes idiomas.

Por fim, vale lembrar que os métodos `toUpperCase()` e `toLowerCase()` são apenas uma das maneiras de capitalizar uma string em Java. Existem outras abordagens disponíveis, como utilizar a classe `StringBuilder` ou expressões regulares, por exemplo.

Veja também:

- [Documentação oficial do Java sobre a classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Outras formas de capitalizar uma string em Java](https://stackabuse.com/how-to-capitalize-strings-in-java/)
- [Guia completo de expressões regulares em Java](https://www.regular-expressions.info/java.html)

---
See Also:

- [Documentação oficial do Java sobre a classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Outras formas de capitalizar uma string em Java](https://stackabuse.com/how-to-capitalize-strings-in-java/)
- [Guia completo de expressões regulares em Java](https://www.regular-expressions.info/java.html)