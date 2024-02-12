---
title:                "Capitalizando uma string"
aliases:
- /pt/arduino/capitalizing-a-string.md
date:                  2024-02-03T19:04:55.868709-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Capitalizar uma string envolve converter o primeiro caractere de cada palavra em uma string para maiúscula enquanto garante que o restante permaneça em minúsculas. Essa operação é comum na formatação de dados e na normalização de entradas do usuário para manter a consistência e melhorar a legibilidade.

## Como fazer:
O Arduino, conhecido principalmente por interagir com hardware, também inclui capacidades básicas de manipulação de strings através do seu objeto `String`. No entanto, ele não possui uma função direta de `capitalizar` como vista em linguagens de nível mais alto. Assim, implementamos a capitalização iterando sobre uma string e aplicando transformações de maiúsculas e minúsculas.

Aqui está um exemplo básico sem usar bibliotecas de terceiros:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Retorna uma string vazia se a entrada estiver vazia
  }
  input.toLowerCase(); // Converte a string inteira para minúscula primeiro
  input.setCharAt(0, input.charAt(0) - 32); // Capitaliza o primeiro caractere
  
  // Capitaliza letras que seguem um espaço
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Saída: "Hello Arduino World"
}

void loop() {
  // Loop vazio
}
```

Este trecho de código define uma função `capitalizeString` que primeiro converte toda a string para minúscula para padronizar seu caso. Em seguida, capitaliza o primeiro caractere e qualquer caractere que siga um espaço, efetivamente capitalizando cada palavra na string de entrada. Note que esta implementação rudimentar assume a codificação de caracteres ASCII e pode precisar de ajustes para suporte completo ao Unicode.

Atualmente, não há bibliotecas de terceiros amplamente adotadas especificamente para manipulação de strings no ecossistema Arduino, principalmente devido ao seu foco na interação com hardware e eficiência. No entanto, o exemplo fornecido é uma maneira direta de alcançar a capitalização de strings dentro do ambiente de programação do Arduino.
