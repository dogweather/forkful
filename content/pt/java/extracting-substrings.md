---
title:    "Java: Extraindo subcadeias"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings é importante?

Extrair substrings, ou partes específicas de uma string, é um processo comum em programação Java. Isso permite que os desenvolvedores acessem dados específicos de uma string de forma rápida e eficiente, economizando tempo e recursos computacionais. Além disso, a extração de substrings é essencial para a manipulação e formatação de dados em muitos aplicativos.

## Como extrair substrings em Java

Extrair substrings em Java é um processo simples e direto. Primeiro, é necessário usar o método `substring()` em um objeto de string. Este método aceita dois parâmetros: o índice inicial e o índice final da substring desejada.

```
//Exemplo de código
String frase = "Esta é uma frase de exemplo";
String substring = frase.substring(5, 14);
System.out.println(substring); // Resultado: "é uma fra"
```

Neste exemplo, a substring começa no índice 5 (incluindo o caractere na posição 5) e termina no índice 13 (excluindo o caractere na posição 14). É importante lembrar que os índices em Java começam em 0, portanto, o índice final deve ser um valor maior do que o índice inicial.

Além disso, também é possível usar o método `substring()` com apenas um parâmetro, isto é, o índice inicial. Isso extrairá todos os caracteres a partir do índice inicial até o final da string.

```
//Exemplo de código
String frase = "Esta é uma frase de exemplo";
String substring = frase.substring(5);
System.out.println(substring); // Resultado: "é uma frase de exemplo"
```

## Aprofundando no processo de extração de substrings

Além dos exemplos de código apresentados, é importante entender alguns conceitos-chave relacionados à extração de substrings em Java.

### Imutabilidade de strings

Em Java, strings são imutáveis, o que significa que não podem ser alteradas após a sua criação. Portanto, sempre que o método `substring()` é chamado, uma nova string é criada com a substring desejada, e a string original permanece inalterada.

### Índices negativos

Em Java, também é possível usar índices negativos ao extrair substrings. Nesse caso, o índice negativo é contado a partir do final da string, em vez do início. Isso é especialmente útil quando não se sabe o comprimento exato da string.

```
//Exemplo de código
String frase = "Esta é uma frase de exemplo";
String substring = frase.substring(-5); // Índice negativo
System.out.println(substring); // Resultado: "Exemplo"
```

### Tratamento de exceções

É importante tratar possíveis exceções ao extrair substrings em Java. Algumas situações comuns que podem causar exceções são índices inválidos (índice final menor que o inicial, por exemplo) e índices fora dos limites da string.

```
//Exemplo de código
String frase = "Esta é uma frase de exemplo";
try {
    String substring = frase.substring(15); // Índice fora dos limites
    System.out.println(substring);
} catch (StringIndexOutOfBoundsException e) {
    e.printStackTrace();
}
```

## Veja também

- [Documentação oficial da classe String em Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial sobre extração de substrings em Java](https://www.baeldung.com/java-string-substring)
- [Vídeo explicativo sobre o método `substring()` em Java](https://www.youtube.com/watch?v=a5KjX2tVej0)

O processo de extração de substrings em Java é uma habilidade importante para qualquer programador dominar. Com o conhecimento adequado e o uso correto dos métodos e conceitos apresentados, a manipulação de strings em seus aplicativos será muito mais eficiente. Experimente diferentes exemplos e pratique essa técnica para aprimorar suas habilidades em Java.