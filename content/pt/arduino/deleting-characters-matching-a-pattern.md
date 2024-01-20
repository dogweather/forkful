---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Excluir caracteres que correspondem a um padrão é uma prática útil em programação para manipular cadeiras de caracteres. Esta ação é frequentemente utilizada para limpar ou formatar strings de entrada.

## Como Fazer:

Aqui está um exemplo simples de como usar a função 'replace' para excluir caracteres que correspondem a um padrão.

```Arduino
   String s = "Olá, Mundo!";
   s.replace("Mundo", "Arduino");
   Serial.println(s);  // Imprime: Olá, Arduino!
```

A função 'replace' procura pela string "Mundo" e a substitui por "Arduino".

## Aprofundando:

(1) Contexto Histórico: As funções de manipulação de strings existem há muito tempo, desde o princípio das linguagens de programação. 

(2) Alternativas: Além da função 'replace', também existem outras funções como 'substring' e 'charAt' que podem ser utilizadas para manipular strings. 

```Arduino
   // Usando a função 'substring'
   String s1 = "Olá, Mundo!";
   String s2 = s1.substring(0,4);
   Serial.println(s2);  // Imprime: Olá,
```

```Arduino
  // Usando a função charAt
  char c = s1.charAt(0);
  Serial.println(c);  // Imprime: O
```

(3) Detalhes de Implementação: Internamente, a função 'replace' utiliza um algoritmo de pesquisa para localizar a sub-string que precisa ser substituída.

## Veja Também:

- [Documentação oficial do Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [W3Schools: Manipulação de String em C++](https://www.w3schools.com/cpp/cpp_strings.asp)

Nota: As fontes estão em inglês. Se necessário, utilize uma ferramenta de tradução.