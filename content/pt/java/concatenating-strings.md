---
title:                "Java: Concatenando strings"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
A concatenação de strings é uma técnica essencial para muitos programadores Java. Com ela, é possível combinar pedaços de texto para criar strings mais complexas e úteis. Mas por que é tão importante dominar essa habilidade? Continue lendo para descobrir!

## Como Fazer
Para concatenar strings em Java, usamos o operador "+" ou o método concat(). Vejamos alguns exemplos:

``` Java
String nome = "João";
String sobrenome = "Silva";
String nomeCompleto = nome + sobrenome; // output: JoãoSilva
String nomeFinal = nome.concat(" ").concat(sobrenome); // output: João Silva
```

Podemos usar também o método StringBuilder para criar strings mutáveis e concatená-las de forma eficiente:

``` Java
StringBuilder frase = new StringBuilder("Eu ").append("amo ").append("programar.");
// output: Eu amo programar.
```

Não se esqueça de usar o método trim() para remover espaços desnecessários ao concatenar strings que contenham espaços.

## Profundidade de Campo
Embora possa parecer simples, a concatenação de strings envolve alguns conceitos importantes. Por exemplo, quando usamos o operador "+", estamos na verdade criando um novo objeto String a partir dos objetos que estamos concatenando. Isso pode ser custoso em termos de desempenho, por isso é recomendado usar o método concat() ou o StringBuilder.

Também é importante lembrar que a ordem de concatenação é importante. Se precisarmos concatenar muitas strings, é mais eficiente usar o método append() do StringBuilder, uma vez que o Java cria um novo objeto String a cada vez que usamos o operador "+".

Entender esses detalhes e aplicar as melhores práticas pode otimizar o desempenho do seu código e tornar a concatenação de strings mais eficiente.

## Veja Também
- [Documentação oficial do Java sobre concatenação de strings](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Guia completo sobre a classe StringBuilder](https://www.javatpoint.com/stringbuilder-class)
- [Vídeo tutorial sobre concatenação de strings em Java](https://www.youtube.com/watch?v=0pTz1Q_G1Ds)