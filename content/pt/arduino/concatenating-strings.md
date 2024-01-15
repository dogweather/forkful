---
title:                "Concatenando strings"
html_title:           "Arduino: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por Que

Concatenar strings é uma técnica de programação muito útil quando se lida com texto no Arduino. Ela permite combinar diferentes strings em uma única string para facilitar a leitura e manipulação de dados.

## Como Fazer

Para concatenar strings no Arduino, você pode usar a função `concat()` da biblioteca `String`. Esta função também permite adicionar outros tipos de dados, como números e caracteres especiais, à string concatenada.

```Arduino
#include <String.h>

void setup() {

  Serial.begin(9600); // Inicia a comunicação serial com a taxa de 9600 bps
  String nome = "João";
  String sobrenome = "Silva";
  String nome_completo = nome.concat(" ", sobrenome); // Concatena as strings e adiciona um espaço entre elas
  
  Serial.println(nome_completo); // Imprime "João Silva" no monitor serial

}

void loop() {
  // vazio
}
```

É importante lembrar que o Arduino tem limitações de memória, por isso é recomendável evitar o uso excessivo de strings longas para não sobrecarregar o sistema.

## Mergulho Profundo

Na linguagem de programação C++, que é utilizada no Arduino, strings são tratadas como arrays de caracteres. Isso significa que elas podem ser acessadas e manipuladas de maneira semelhante a outros tipos de arrays.

Para concatenar strings manualmente, sem o uso da função `concat()`, podemos utilizar a função `strcat()` da biblioteca `cstring`. Esta função possui a seguinte sintaxe: `strcat(destino, origem)`, onde `destino` é a string à qual será adicionada a string `origem`.

```Arduino
#include <cstring>

void setup() {

  Serial.begin(9600);
  char destino[30] = "Olá";
  char origem[] = " mundo!";
  strcat(destino, origem); // Adiciona a string " mundo!" à string "Olá"
  
  Serial.println(destino); // Imprime "Olá mundo!" no monitor serial

}

void loop() {
  // vazio
}
```

Além disso, também existe a função `sprintf()`, que permite formatar a concatenação de strings com variáveis adicionais. Esta função é semelhante ao `printf()` da linguagem C e possui a seguinte sintaxe: `sprintf(destino, "string", variáveis)`, onde `destino` é a string à qual será adicionada a string formatada.

```Arduino
void setup() {

  Serial.begin(9600);
  char destino[30];
  int idade = 30;
  sprintf(destino, "Eu tenho %d anos.", idade); // Formata a string com a variável idade
  
  Serial.println(destino); // Imprime "Eu tenho 30 anos." no monitor serial

}

void loop() {
  // vazio
}
```

## Veja Também

- [Documentação da função `concat()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Documentação da biblioteca `cstring`](https://www.cplusplus.com/reference/cstring/)
- [Documentação da função `sprintf()`](https://www.cplusplus.com/reference/cstdio/sprintf/)