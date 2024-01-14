---
title:    "Arduino: Concatenando strings"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenar strings é uma habilidade importante ao programar em Arduino, pois permite a criação de mensagens e informações mais complexas a partir de várias partes menores. Isso pode ser útil em projetos que envolvem exibição de dados em um display ou comunicação com outros dispositivos.

## How To

Para concatenar strings no Arduino, nós utilizamos a função `concat()` da biblioteca `String`. Veja um exemplo abaixo:

```Arduino
// Declarando duas variáveis do tipo String
String nome = "João";
String sobrenome = "Silva";

// Concatenando as strings
String nomeCompleto = nome.concat(sobrenome);

// Imprimindo o resultado
Serial.println(nomeCompleto); // Saída: João Silva
```

No exemplo acima, utilizamos a função `concat()` para juntar as strings "João" e "Silva" em uma única variável chamada `nomeCompleto`. Essa variável, por sua vez, pode ser utilizada em outras partes do código, como por exemplo, para exibir o nome completo em um display LCD.

É importante ressaltar que a função `concat()` não altera o valor das variáveis originais, apenas cria uma nova variável com a string concatenada.

Além disso, é possível também concatenar mais de duas strings ao mesmo tempo, basta adicioná-las como parâmetros dentro da função `concat()`. Veja o exemplo abaixo:

```Arduino
// Declarando três variáveis do tipo String
String nome = "João";
String sobrenome = "da Silva";
String apelido = "Bolacha";

// Concatenando as strings
String perfil = nome.concat(sobrenome, " (apelido: ", apelido, ")");

// Imprimindo o resultado
Serial.println(perfil); // Saída: João da Silva (apelido: Bolacha)
```

Nesse segundo exemplo, concatenamos as três strings e adicionamos alguns caracteres extras, criando uma mensagem mais elaborada.

## Deep Dive

Concatenar strings pode parecer uma tarefa simples, mas é importante entender como essa operação funciona por trás dos bastidores. Em Arduino, as strings são tratadas como objetos do tipo `String`, que possuem vários métodos e propriedades para manipulação.

Quando utilizamos a função `concat()`, o Arduino cria uma nova string maior e copia todo o conteúdo das strings originais para essa nova string. Isso pode causar um consumo maior de memória, especialmente se as strings forem grandes.

Portanto, é importante utilizar a concatenação de strings com cautela e evitar o uso desnecessário dessa operação. Em alguns casos, pode ser mais eficiente utilizar outras técnicas, como por exemplo, o uso de caracteres coringa (%d, %s, %f) em uma string formatada.

## See Also

- [Referência da função concat() da biblioteca String](https://www.arduino.cc/reference/pt/libraries/string/object/concat/)
- [Exemplos de uso da função concat()](https://www.arduino.cc/en/Tutorial/StringAdditionalOperators)
- [Tutorial sobre formatação de strings em Arduino](https://www.arduino.cc/en/Tutorial/TextStringformatting)