---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:43.926143-07:00
description: "Capitalizar uma string envolve modificar a primeira letra de cada palavra\
  \ na string para mai\xFAscula enquanto assegura que o restante permane\xE7a em\u2026"
lastmod: '2024-03-13T22:44:46.439866-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string envolve modificar a primeira letra de cada palavra\
  \ na string para mai\xFAscula enquanto assegura que o restante permane\xE7a em min\xFA\
  scula."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
A biblioteca padrão do Java não fornece um método direto para capitalizar strings inteiras de uma vez, mas você pode realizar isso com uma combinação de métodos integrados. Para necessidades mais sofisticadas, bibliotecas de terceiros como Apache Commons Lang oferecem soluções diretas.

### Usando Métodos Internos do Java
Para capitalizar uma string sem bibliotecas externas, você pode dividir a string em palavras, capitalizar a primeira letra de cada uma e depois juntá-las novamente. Aqui está uma abordagem simples:
```java
public class CapitalizeString {
    public static void main(String[] args) {
        String texto = "olá, mundo!";
        String textoCapitalizado = capitalizeWords(texto);
        System.out.println(textoCapitalizado); // Saída: "Olá, Mundo!"
    }

    public static String capitalizeWords(String str) {
        char[] caracteres = str.toLowerCase().toCharArray();
        boolean encontrado = false;
        for (int i = 0; i < caracteres.length; i++) {
            if (!encontrado && Character.isLetter(caracteres[i])) {
                caracteres[i] = Character.toUpperCase(caracteres[i]);
                encontrado = true;
            } else if (Character.isWhitespace(caracteres[i]) || caracteres[i]=='.' || caracteres[i]=='\'') { 
                encontrado = false;
            }
        }
        return String.valueOf(caracteres);
    }
}
```
Este trecho de código converte a string inteira para minúscula e depois itera por cada caractere, capitalizando a primeira letra de cada palavra. Ele considera espaços, pontos e apóstrofos como separadores de palavras.

### Usando Apache Commons Lang
A biblioteca Apache Commons Lang fornece uma solução mais elegante com o método `WordUtils.capitalizeFully()`, que lida com vários casos extremos e delimitadores para você:
```java
// Adicione a dependência: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String texto = "olá, mundo!";
        String textoCapitalizado = WordUtils.capitalizeFully(texto);
        System.out.println(textoCapitalizado); // Saída: "Olá, Mundo!"
    }
}
```

Para usar este método, você precisará adicionar a biblioteca Apache Commons Lang ao seu projeto. Este método da biblioteca não apenas capitaliza a primeira letra de cada palavra, mas também converte o restante das letras em cada palavra para minúscula, assegurando um padrão de capitalização consistente em toda a string.
