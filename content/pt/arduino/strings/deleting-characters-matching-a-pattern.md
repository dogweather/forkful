---
date: 2024-01-20 17:41:55.769499-07:00
description: "How to: (Como Fazer:) Vamos l\xE1! No Arduino, n\xE3o temos uma fun\xE7\
  \xE3o pronta pra isso, ent\xE3o vamos criar uma. Olha s\xF3 este exemplo, que remove\
  \ todos os d\xEDgitos\u2026"
lastmod: '2024-04-05T22:38:46.002574-06:00'
model: gpt-4-1106-preview
summary: "(Como Fazer:) Vamos l\xE1!."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## How to: (Como Fazer:)
Vamos lá! No Arduino, não temos uma função pronta pra isso, então vamos criar uma. Olha só este exemplo, que remove todos os dígitos de uma string:

```Arduino
void setup() {
  Serial.begin(9600);
  
  String original = "Ardu1n0 R0ck5!";
  String cleaned = deleteMatchingCharacters(original, '0'-'9');
  
  Serial.println(cleaned); // Mostrará: "Arduino Rcks!"
}

void loop() {
  // Nada aqui por enquanto
}

String deleteMatchingCharacters(String str, char fromChar, char toChar) {
  String result = "";
  for (char c : str) {
    if (c < fromChar || c > toChar) {
      result += c;
    }
  }
  return result;
}
```
A função `deleteMatchingCharacters` tira os caracteres que estão no intervalo de '0' a '9'. Modifique os parâmetros se precisar de outro padrão.

## Deep Dive (Mergulho Profundo)
Historicamente, linguagens de programação de alto nível como Python ou JavaScript têm funções embutidas para manipular strings de maneira sofisticada. No Arduino, que usa C/C++, tais funções são mais escassas ou inexistentes, levando a criações manuais como a nossa função `deleteMatchingCharacters`.

Alternativas? Podemos usar expressões regulares com bibliotecas adicionais, mas isso consome mais memória, algo sempre a se pensar em sistemas embarcados.

Sobre implementação, ao criar funções como `deleteMatchingCharacters`, lembre-se da eficiência. Manipulações de strings podem ser custosas. Nossa função cria uma nova string ao invés de alterar a original, uma escolha que pode afetar a performance se as strings forem grandes ou as operações frequentes.

## See Also (Veja Também)
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Forum: String Manipulation](https://forum.arduino.cc/index.php?board=9.0)
- [Wikipedia: Regular Expression](https://pt.wikipedia.org/wiki/Express%C3%A3o_regular) - para entender mais sobre padrões de caracteres e como eles funcionam em outras linguagens.

Lembrando que é sempre bom dar uma olhada nos fóruns do Arduino e na documentação oficial para mais exemplos e discussões sobre manipulação de strings e padrões.
