---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que é e Porquê?
Iniciar um novo projeto de programação é basicamente esboçar a primeira infraestrutura para um aplicativo ou programa. Os programadores fazem isso para estabelecer um ponto inicial, um esqueleto sobre o qual eles constroem o programa em si.

## Como fazer: 
Para começar um novo projeto em Clojure, você precisará do pacote de gerenciamento de projetos de Clojure, ou Leiningen.

Por exemplo:

```Clojure
lein new meu-projeto
```

Depois de executar, você observará uma estrutura de diretórios recém-criada em 'meu-projeto'. É aqui que você começará a programar.

## Mergulho Profundo 
Clojure, uma linguagem de programação dinâmica funcional, lançada em 2007, tem raízes em Lisp e roda na máquina virtual Java (JVM). Para começar um novo projeto, poderíamos também considerar alternativas à abordagem Leiningen, como utilizando o Boot ou o CLI/deps do Clojure.

No entanto, o Leiningen é amplamente preferido devido à sua facilidade de uso e à capacidade de manipular um projeto inteiro, desde a criação até a execução dos testes e a geração de distribuições autônomas. Ao criar um novo projeto, ele estabelece uma estrutura de diretório ideal que é eficiente e fácil de navegar, poupa tempo e permite que os programadores se concentrem na lógica e na funcionalidade do programa em si.

## Veja Também
- Documentação oficial da Clojure: [https://clojure.org/](https://clojure.org/)
- Documentação oficial do Leiningen: [https://leiningen.org/](https://leiningen.org/)
- Introdução ao Clojure: [https://www.braveclojure.com/](https://www.braveclojure.com/)