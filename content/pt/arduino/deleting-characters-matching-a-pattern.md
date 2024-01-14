---
title:                "Arduino: Excluindo caracteres que correspondem a um padrão"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Ao trabalhar com programação, é comum que nos deparemos com situações em que precisamos manipular strings e filtrar informações específicas dentro delas. Nesses casos, a exclusão de caracteres que correspondem a um determinado padrão pode ser uma ferramenta extremamente útil e eficiente para atingir o resultado desejado. Em Arduino, essa tarefa pode ser realizada de forma simples e eficaz utilizando algumas funções e comandos específicos.

## Como fazer:

Para deletar caracteres que seguem um determinado padrão em uma string no Arduino, podemos utilizar a função `replaceAll()` juntamente com a função `String()` para converter a variável em uma string e realizar a manipulação necessária.  Veja um exemplo de código abaixo:

```
void setup() {
  Serial.begin(9600);
  String texto = "Teste123abc";
  Serial.println("Texto original: " + texto);
  texto = String(texto).replaceAll("[0-9]", ""); //Apaga todos os números da string
  Serial.println("Texto com números apagados: " + texto);
}

void loop() {
  //vazio
}
```

Nesse exemplo, o resultado final seria a string "Testeabc", pois a função `replaceAll()` funcionaria da seguinte forma: o primeiro parâmetro é o padrão a ser procurado - no caso, qualquer número entre 0 e 9 -, e o segundo parâmetro é o que será substituído. Como deixamos o segundo parâmetro vazio, os números serão simplesmente apagados da string.

## Profundidade:

Para uma compreensão mais aprofundada sobre o funcionamento da função `replaceAll()`, é importante entender como ela trabalha com a expressão regular utilizada no primeiro parâmetro. Essa expressão pode ser usada para especificar um conjunto de caracteres que serão procurados e substituídos.

Uma expressão entre colchetes `[...]` significa que qualquer um dos caracteres que estiverem dentro dos colchetes será uma correspondência. Por exemplo, `[0-9]` significa que qualquer número de 0 a 9 será considerado uma correspondência. Além disso, podemos utilizar o caractere `^` para indicar a negação da expressão, ou seja, ela irá buscar por todos os caracteres diferentes dos especificados. Por exemplo, `[^A-Za-z]` irá buscar por todos os caracteres que não sejam letras maiúsculas ou minúsculas.

Outra forma de utilizar a função `replaceAll()` é especificando uma sequência de caracteres que será procurada e substituída. Por exemplo, `replaceAll("tes", "carro")` irá substituir todas as ocorrências de "tes" por "carro" na string.

## Veja também:

- Documentação oficial da [função replaceAll()](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/replaceall/)
- [Tutorial sobre expressões regulares em Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/expression-regular-express-es-re-gulares-105f30)
- [Vídeo explicativo sobre a função replaceAll()](https://www.youtube.com/watch?v=6iZKrBZsHEk)