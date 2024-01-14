---
title:                "Clojure: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto pode ser útil para armazenar informações e dados importantes de forma organizada e fácil de acessar. Além disso, pode ser uma maneira eficiente de compartilhar informações com outras pessoas ou sistemas.

## Como fazer:

Para escrever um arquivo de texto em Clojure, você pode usar a função `spit`. Ela permite que você escreva uma string em um arquivo especificado, como mostrado no exemplo abaixo:

```Clojure
(spit "exemplo.txt" "Olá mundo!")
```

Isso criaria um arquivo de texto chamado "exemplo.txt" com a frase "Olá mundo!" dentro dele. Além disso, você também pode usar a função `slurp` para ler o conteúdo de um arquivo de texto.

```Clojure
(slurp "exemplo.txt")
```

Isso retornaria a string "Olá mundo!".

## Explorando mais a fundo:

Além das funções `spit` e `slurp`, existem outras maneiras de trabalhar com arquivos de texto em Clojure, como usar bibliotecas específicas ou utilizar algumas funções mais avançadas da linguagem. É importante pesquisar e experimentar para descobrir a melhor abordagem para o seu projeto específico.

## Veja também:

Aqui estão alguns links úteis para saber mais sobre como trabalhar com arquivos de texto em Clojure:

- [Documentação oficial da função `spit`](https://clojuredocs.org/clojure.core/spit)
- [Documentação oficial da função `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [Exemplo de uso de biblioteca específica para manipular arquivos de texto em Clojure](https://github.com/clojure-cookbook/clojure-cookbook/tree/master/04_file-io/4-03_reading-a-file-into-a-string)