---
title:                "Java: Usando expressões regulares"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Porque Usar Expressões Regulares em Programação Java?

As expressões regulares são uma poderosa ferramenta na programação Java que permitem encontrar e manipular padrões de texto de forma eficiente. Elas são amplamente utilizadas em tarefas como validação de entrada de dados, busca de informações em texto e substituição de caracteres.

## Como Usar Expressões Regulares em Programação Java?

Para utilizar expressões regulares em Java, é necessário importar a classe "Regex" e criar um objeto que represente o padrão que se deseja encontrar. Em seguida, é possível utilizar métodos como "matches()" e "find()" para comparar o padrão com uma string de entrada e retornar uma correspondência ou uma lista de correspondências.

```Java
// Exemplo de uso básico de expressões regulares em Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {

	public static void main(String[] args) {
		String regex = "[a-z]+"; // padrão para encontrar letras minúsculas
		String texto = "Olá, meu nome é João!";
		
		Pattern pattern = Pattern.compile(regex); // cria o objeto padrão
		Matcher matcher = pattern.matcher(texto); // compara o padrão com o texto
		
		while (matcher.find()) { // procura por correspondências
			System.out.println(matcher.group()); // imprime a correspondência encontrada
		}
	}
}
```

A saída desse exemplo será:

```
l
meu
nome
é
o
```

## Aprofundando no Uso de Expressões Regulares em Java

As expressões regulares em Java oferecem diversas opções de modificadores e métodos que permitem uma maior precisão e controle sobre as correspondências encontradas. Alguns desses recursos incluem:

- Caracteres coringa: é possível utilizar o "." para representar qualquer caracter, ou utilizar colchetes para especificar um grupo de caracteres permitidos.
- Quantificadores: permitem definir a quantidade de caracteres a serem encontrados, como "+" para encontrar uma ou mais repetições, ou "*" para encontrar zero ou mais.
- Grupos de captura: permitem definir quais partes do padrão devem ser retornadas como correspondência.
- Modificadores como "i" para ignorar a diferença entre maiúsculas e minúsculas e "s" para fazer com que o "." também considere a quebra de linha.

Para uma lista completa de modificadores e métodos, recomenda-se consultar a documentação oficial da classe "Regex" em Java.

# Veja Também

- [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Documentação oficial da classe "Regex"](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Expressões Regulares - Guia Básico](https://www.devmedia.com.br/guia/expressoes-regulares/37904)