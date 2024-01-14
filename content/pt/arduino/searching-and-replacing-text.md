---
title:                "Arduino: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Se você é um amante da tecnologia e gosta de explorar diferentes projetos de eletrônica, provavelmente já teve que lidar com a necessidade de substituir textos em seus códigos Arduino. Este processo pode ser útil para fazer alterações rápidas ou para facilitar a compreensão do código. Portanto, saber como realizar essa tarefa pode ser muito útil.

## Como fazer

Substituir textos em um código Arduino é uma tarefa relativamente simples, graças às funções incorporadas na linguagem. Para realizar essa ação, você precisará da função ```replace()```, que recebe três argumentos: a variável que contém o texto original, o texto a ser substituído e o novo texto a ser inserido. Vamos dar uma olhada em um exemplo prático:

```
String texto = "Oi, mundo!";
texto.replace("oi", "Olá");
Serial.println(texto);
```

Neste código, a palavra "Oi" será substituída por "Olá", resultando na seguinte saída: "Olá, mundo!". É importante notar que a função ```replace()``` é case-sensitive, então "oi" e "Oi" são considerados textos diferentes e só serão substituídos se estiverem escritos da mesma maneira.

## Mergulho profundo

A função ```replace()``` também aceita expressões regulares como argumentos, o que a torna ainda mais poderosa. Além disso, se você precisar substituir todos os casos de uma determinada palavra ou frase, é possível usar a função ```replaceAll()```, que funciona de maneira semelhante à ```replace()```, mas faz a substituição em todos os casos.

Outro detalhe importante é que a função ```replace()``` retorna um valor booleano, indicando se a substituição foi bem-sucedida ou não. Portanto, se você precisar realizar alguma ação com base nesse resultado, é possível armazená-lo em uma variável e usá-lo em suas condições condicionais.

## Veja também

Aqui estão alguns links úteis com mais informações sobre substituição de texto em códigos Arduino:

- [Documentação oficial do Arduino sobre a função replace()](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/replace/)
- [Tutorial em vídeo sobre como substituir texto em códigos Arduino](https://www.youtube.com/watch?v=-3KUTF0KIYM)
- [Artigo sobre expressões regulares em Arduino](https://www.automotioncomponents.co.uk/blog/using-regular-expressions-with-the-arduino-programming-language/)