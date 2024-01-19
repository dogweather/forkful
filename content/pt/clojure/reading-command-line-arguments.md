---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Ler argumentos da linha de comando permite aos programas receberem input diretamente no início do processo. Programadores usam isso para personalizar o comportamento do programa com base na entrada do usuário ou no ambiente.

## Como fazer:
Se você precisa acessar os argumentos da linha de comando em Clojure, você pode usar a variável pré-definida `*command-line-args*`. Aqui está um exemplo e sua saída:

```Clojure
(defn -main
   [& args]
   (println "Os argumentos da linha de comando são:" args))

; Saída quando executado com: lein run arg1 arg2
; Os argumentos da linha de comando são: (arg1 arg2)
```

## Mergulho Profundo
1. **Contexto histórico**: As linguagens só precisam oferecer funcionalidades para ler argumentos da linha de comando porque a interação com a linha de comando já foi (e ainda é) uma forma comum de se usar sistemas de computador. Essa funcionalidade é herança do estilo Unix de desenvolvimento de software.

2. **Alternativas**: O `clojure.tools.cli` é um pacote que fornece funções para analisar os argumentos da linha de comando. Ele é mais flexível e poderoso que o `*command-line-args*`, tornando mais fácil a criação de interfaces de linha de comando complexas.

3. **Detalhes de implementação**: A variável `*command-line-args*` está disponível apenas no script quando ele é iniciado com `lein run`ou um uberjar. Para o REPL ou outro ambiente, você precisará usar uma alternativa.

## Veja Também
1. Documentação oficial do Clojure: [https://clojure.org/reference/vars](https://clojure.org/reference/vars)
2. clojure.tools.cli no GitHub: [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)