---
title:                "Escrevendo no erro padrão"
html_title:           "Kotlin: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever para o erro padrão, também conhecido como stderr, é quando os programadores optam por enviar mensagens de erro para uma saída separada em vez da saída padrão. Isso é feito para distinguir mensagens de erro de mensagens de saída, tornando mais fácil para os usuários identificarem e corrigirem problemas em seus códigos.

## Como fazer:

```Kotlin
fun main() {
    System.err.println("Este é um exemplo de mensagem de erro")
}
```

Saída:
```
Este é um exemplo de mensagem de erro
```

## Profundando um pouco mais:

No passado, mensagens de erro costumavam ser enviadas para a saída padrão, o que tornava mais difícil para os usuários encontrarem e corrigirem problemas em seus códigos. Felizmente, usar a saída padrão do sistema operacional para mensagens de erro foi abandonado em favor do stderr. Além disso, existem alternativas para a escrita para o stderr, como gravar em um arquivo de log.

Além disso, a implementação da escrita para o stderr depende do sistema operacional e da linguagem de programação utilizada. Por exemplo, em Kotlin, podemos usar o objeto System.err para acessar o stderr enquanto em outras linguagens, como C++, podemos usar a função std::cerr.

## Veja também:
