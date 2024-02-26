---
date: 2024-01-20 17:46:14.847777-07:00
description: "Extrair substrings significa pegar peda\xE7os espec\xEDficos de uma\
  \ string. Programadores fazem isso para manipular, analisar ou comparar partes de\
  \ texto de\u2026"
lastmod: '2024-02-25T18:49:44.066427-07:00'
model: gpt-4-1106-preview
summary: "Extrair substrings significa pegar peda\xE7os espec\xEDficos de uma string.\
  \ Programadores fazem isso para manipular, analisar ou comparar partes de texto\
  \ de\u2026"
title: Extraindo substrings
---

{{< edit_this_page >}}

## What & Why?
Extrair substrings significa pegar pedaços específicos de uma string. Programadores fazem isso para manipular, analisar ou comparar partes de texto de forma eficiente.

## How to:
```Java
public class ExtractorDeSubstring {
    public static void main(String[] args) {
        String texto = "O café está quente.";
        // Extrair a palavra "café"
        String cafe = texto.substring(2, 6);
        System.out.println(cafe); // Saída: café
        
        // Extrair a palavra "quente"
        String quente = texto.substring(12);
        System.out.println(quente); // Saída: quente
    }
}
```
Execute o código acima e você vai ver as partes "café" e "quente" sendo impressas na tela.

## Deep Dive
A capacidade de extrair substrings existe desde as primeiras versões do Java. Originalmente, a classe `String` oferecia métodos `substring()` que funcionavam criando uma nova string com os mesmos caracteres da string original, a partir de um índice inicial até um final. Isso mudou um pouco para evitar desperdício de memória e agora cria uma nova string que compartilha o mesmo array de caracteres interno, desde que isso seja possível.

Existem alternativas para extrair substrings, como usar `String.split()`, `StringTokenizer` ou expressões regulares com a classe `Pattern`. No entanto, `substring()` é geralmente o caminho mais direto e eficiente quando você sabe exatamente os índices dos caracteres que deseja.

Quanto à implementação, até Java 6, o método `substring()` compartilhava o array de caracteres da string original, o que podia levar a problemas de memória se a string original fosse muito grande e a substring muito pequena. Esse comportamento mudou a partir do Java 7 (update 6), onde a chamada de `substring()` resulta em uma cópia do range de caracteres desejado, evitando assim a retenção não intencional de grandes arrays de caracteres.

## See Also
- Documentação oficial da classe String e método substring(): https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int)
- Tutorial de Java da Oracle sobre Strings: https://docs.oracle.com/javase/tutorial/java/data/strings.html
- Discussões sobre o uso de `substring()`: https://stackoverflow.com/questions/tagged/java+substring
