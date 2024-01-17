---
title:                "Procurando e substituindo texto"
html_title:           "Arduino: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e Por que?

A busca e substituicao de texto e uma tecnologia utilizada por programadores para encontrar e alterar pedacos de texto dentro do codigo de forma automatica. E uma maneira eficiente de fazer alteracoes em massa sem ter que fazer manualmente, economizando tempo e esforco.

## Como fazer:

### Exemplo 1: Buscando e Substituindo em uma String

```Arduino
// Declarando a string
String texto = "Ola mundo!";

// Buscando e substituindo o "Ola" por "Oi"
texto.replaceAll("Ola", "Oi");

// Impressao do texto alterado
Serial.println(texto);
// Output: Oi mundo!
```

### Exemplo 2: Buscando e Substituindo em um Array

```Arduino
// Declarando o array
String nomes[] = {"Joao", "Maria", "Pedro"};

// Buscando e substituindo "Maria" por "Mariana"
for(int i = 0; i < 3; i++){
  nomes[i].replaceAll("Maria", "Mariana");
}

// Impressao do array alterado
for(int i = 0; i < 3; i++){
  Serial.println(nomes[i]);
}
// Output: Joao Mariana Pedro
```

### Exemplo 3: Buscando e Substituindo com Expressoes Regulares

```Arduino
// Declarando a string
String frase = "Gosto de gatos e cachorros!";

// Usando uma expressao regular para substituir todas as vogais por "u"
frase.replaceAll("[aeiou]", "u");

// Impressao do texto alterado
Serial.println(frase);
// Output: Gusu du gutus u cuchurrus!
```

## Aprofundando:

### Contexto Historico:

A busca e substituicao de texto tem suas origens nos editores de texto do inicio da computacao. Com o crescimento da industria de software, essa tecnologia foi sendo adotada por linguagens de programacao, facilitando o processo de desenvolvimento de softwares e automacao de tarefas repetitivas.

### Alternativas:

Algumas alternativas para busca e substituicao de texto em programacao incluem o uso de expressoes regulares, linguagens de script como Python e ferramentas de edicao de texto avancadas, como o Vim e o Sublime Text.

### Detalhes de Implementacao:

A busca e substituicao de texto em Arduino e realizada utilizando o metodo `replaceAll()` da classe `String`, que procura por todas as ocorrencias do valor especificado e substitui por outro valor. E importante utilizar o metodo `replaceAll()` em vez de `replace()` para garantir que todas as ocorrencias sejam substituidas.

## Veja Tambem:

- [Documentacao Oficial Arduino - Metodo `replace()`](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/replace/)
- [Wikipedia - Expressoes Regulares](https://pt.wikipedia.org/wiki/Express%C3%A3o_regular)
- [Tutorial de Expressoes Regulares em Python](https://www.tutorialspoint.com/python/python_reg_expressions.htm)