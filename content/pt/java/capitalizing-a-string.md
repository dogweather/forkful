---
title:    "Java: Transformando uma string em maiúsculo"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string é importante
Capitalizar uma string é uma ação simples e essencial para garantir que as palavras sejam apresentadas corretamente. Ao capitalizar uma string, você garante que as primeiras letras de cada palavra estejam em maiúsculo, o que facilita a leitura e a compreensão de um texto.

## Como capitalizar uma string em Java
Aqui estão alguns exemplos em Java de como capitalizar uma string:

```Java
String texto = "aprendendo programação";
String textoCapitalizado = texto.substring(0, 1).toUpperCase() + texto.substring(1);
System.out.println(textoCapitalizado);

// Saída: Aprendendo programação
```

Neste exemplo, usamos o método `substring()` para obter a primeira letra da string original e convertê-la em maiúscula, e depois concatenamos com o restante da string.

Outra opção é usar a classe `java.util.Locale` para capitalizar a string, conforme mostrado no exemplo a seguir:

```Java
String texto = "programando em Java";
String textoCapitalizado = texto.toUpperCase(Locale.getDefault());
System.out.println(textoCapitalizado);

// Saída: PROGRAMANDO EM JAVA
```

## Profundidade do processo de capitalização
Existem diferentes formas de capitalizar uma string, e as duas mostradas acima são apenas algumas delas. O importante é entender as diferentes opções e escolher a mais adequada para cada situação.

Além disso, é importante lembrar que a capitalização de strings também depende da linguagem utilizada. Em alguns idiomas, como o alemão, existem regras específicas para a capitalização de palavras.

Portanto, é fundamental que os desenvolvedores tenham conhecimento sobre as particularidades linguísticas e as melhores práticas ao lidar com strings em seus projetos.

## Veja também
- [Documentação oficial do Java sobre o método toUpperCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Tutorial sobre como capitalizar strings em Java](https://www.baeldung.com/java-capitalize-string)