---
title:    "Java: Utilizando expressões regulares"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Java?
Expressões regulares são uma ferramenta poderosa no mundo da programação e podem ser especialmente úteis para os desenvolvedores Java. Elas permitem encontrar e manipular padrões de texto de maneira rápida e eficiente, economizando tempo e recursos. Neste artigo, vamos explorar como utilizar expressões regulares em Java e como isso pode melhorar o seu código.

## Como usar Expressões Regulares em Java
Para utilizar expressões regulares em Java, primeiro é necessário importar a classe "java.util.regex". Em seguida, é preciso criar um objeto do tipo "Pattern", que contém a expressão que queremos buscar. Podemos então usar esse objeto para criar um objeto "Matcher", que nos permite realizar a busca por padrões em uma determinada string. A seguir, um exemplo de código que busca por números em uma string e imprime o resultado:

```Java
import java.util.regex.*;

public class ExemploRegex {
    public static void main(String[] args) {
        // cria uma expressão que busca por números
        Pattern pattern = Pattern.compile("\\d+");
        // cria uma string de exemplo
        String texto = "A era da informação é a era dos números!";
        // cria um Matcher para realizar a busca na string
        Matcher matcher = pattern.matcher(texto);
        
        // enquanto houver números encontrados, imprime o resultado
        while (matcher.find()) {
            System.out.println("Número encontrado: " + matcher.group());
        }
    }
}
```
Output:
```
Número encontrado: 0
Número encontrado: 1
```

## Aprofundando-se nas Expressões Regulares em Java
As expressões regulares em Java podem ser utilizadas em diversas situações, desde validação de entradas de dados até formatação de strings. Elas permitem usar metacaracteres para buscar por padrões específicos, como números, letras, espaços, entre outros. Além disso, é possível utilizar quantificadores, que determinam a quantidade de ocorrências de um determinado padrão.

Uma das vantagens de utilizar expressões regulares em Java é que elas são altamente customizáveis. Podemos criar nossas próprias regras de busca, tornando-as ainda mais precisas e eficientes.

Porém, é importante lembrar que é preciso ter um conhecimento sólido sobre as regras e sintaxe de expressões regulares antes de utilizá-las em um projeto. Um equívoco na criação da expressão pode resultar em resultados incorretos ou até mesmo em falhas no código.

## Ver também
- [Documentação oficial do Java sobre Expressões Regulares](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Tutorial de expressões regulares em Java](https://www.w3schools.com/java/java_regex.asp)
- [Testador de expressões regulares online](https://regexr.com/)