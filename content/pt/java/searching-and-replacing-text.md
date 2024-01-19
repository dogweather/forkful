---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Buscar e Substituir Texto em Java

## O Que & Por Quê?

A busca e substituição de texto são operações fundamentais em programação que encontram e trocam partes específicas de strings por novos textos. Realizamos isso quando precisamos alterar um padrão recorrente, corrigir erros ou atualizar informações.

## Como Fazer:

Uma maneira simples de realizar essas operações em Java é utilizando o método `replace()` de String. Veja este exemplo:

```Java
public class Main {
  public static void main(String[] args) {
    String texto = "O rato roeu a roupa do rei de Roma.";
    String novoTexto = texto.replace("rato", "gato");
    System.out.println(novoTexto);
  }
}
```

A saída será: `O gato roeu a roupa do rei de Roma.`

No exemplo acima, substituímos todas as ocorrências da palavra "rato" por "gato".

## Mergulho Profundo

Embora o método `replace()` seja a maneira mais comum e direta de realizar a busca e substituição, não é a única opção disponível em Java. Na verdade, este método tem suas raízes na Classes `StringBuffer` e `StringBuilder`, presentes desde as primeiras versões de Java, que possuem o método `replace()`.

Existem alternativas mais poderosas, como a classe `Pattern` do pacote `java.util.regex`, que permite buscar e substituir utilizando expressões regulares. Esta é uma opção mais flexível e avançada, mas também é mais complexa.

Importante destacar que, em Java, as strings são imutáveis. Isso significa que quando você "substitui" uma parte de uma string, está de fato criando uma nova string com os caracteres alterados. Isso pode ter implicações ao lidar com grandes volumes de texto, devido ao consumo de memória.

## Veja Também

Para maiores detalhes sobre a busca e substituição de textos em Java, podem ser úteis os seguintes links:

- [Documentação oficial de Java para a classe String](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [Documentação oficial de Java para a classe Pattern](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/regex/Pattern.html)
- [Oracle's Java Tutorials: Manipulação de Texto com a Classe String](https://docs.oracle.com/javase/tutorials/index.html)