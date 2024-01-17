---
title:                "Criando um arquivo temporário"
html_title:           "Kotlin: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e por que?

Criar um arquivo temporário em um programa Kotlin é uma forma de armazenar informações temporárias que serão usadas apenas durante a execução do programa. Isso é útil para evitar a sobrecarga do sistema com arquivos desnecessários e para manter a privacidade dos dados do usuário. Programadores muitas vezes criam arquivos temporários ao executarem tarefas como transferência de arquivos ou geração de relatórios.

## Como fazer:

```
Kotlin val tempFile = createTempFile() tempFile.writeText("Hello World!") 
```

Output: Um novo arquivo temporário chamado "tempFile" será criado e a mensagem "Hello World!" será escrita nele.

## Profundando:

Criar arquivos temporários é uma prática comum na programação e pode ser encontrada em diversas linguagens de programação. Além do exemplo apresentado aqui, há outras formas de criar arquivos temporários em Kotlin, como através da biblioteca "java.io" ou com a declaração de blocos de código temporários.

Programadores podem optar por criar arquivos temporários quando precisam manipular dados sensíveis ou ao executarem tarefas que resultam em grandes quantidades de dados. Além disso, arquivos temporários também podem ser utilizados para fins de teste e depuração de código.

## Veja também:

Para mais informações sobre a criação de arquivos temporários em Kotlin, recomendamos a leitura da documentação oficial da linguagem e a busca por exemplos de código na comunidade de desenvolvedores. Além disso, outras fontes confiáveis como blogs e fóruns também podem ser úteis.