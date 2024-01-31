---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string significa converter todas as letras dela para maiúsculas. Fazemos isso para padronizar dados, melhorar a legibilidade ou para necessidades específicas de um problema de programação.

## How to:
Capitalize um texto no Arduino é simples. Veja esse exemplo de código:

```Arduino
void setup() {
  Serial.begin(9600);
  String titulo = "isso é um exemplo";
  titulo.toUpperCase();
  Serial.println(titulo);
}

void loop() {
  // Aqui o código a repetir, se necessário.
}
```

Saída esperada no Serial Monitor:

```
ISSO É UM EXEMPLO
```

## Deep Dive
Capitalizar strings não é uma invenção moderna. Desde os primeiros computadores, os programadores precisam manipular texto. Métodos como `toUpperCase()` no Arduino são herdados de linguagens mais antigas, como C.

Existem alternativas, como percorrer cada caractere da string e converter manualmente utilizando a tabela ASCII. Entretanto, isso é recriar a roda.

Sobre a implementação, o `toUpperCase()` do Arduino modifica a string original. Se precisar manter a versão original, armazene-a em outra variável antes de capitalizar.

## See Also
- Documentação oficial do Arduino sobre Strings: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tabela ASCII para referência manual de caracteres: https://www.asciitable.com/
- Discussões sobre boas práticas de manipulação de strings: https://stackoverflow.com/questions/tagged/arduino+string
