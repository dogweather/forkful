---
title:                "Clojure: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Por que iniciar um novo projeto em Clojure?

Iniciar um novo projeto em Clojure pode ser uma decisão sábia para qualquer programador que busca uma linguagem de programação moderna e poderosa. Com sua sintaxe simples e funcional, Clojure permite que desenvolvedores criem aplicativos robustos e escaláveis de forma eficiente.

# Como começar em Clojure?

Começar um novo projeto em Clojure é fácil e rápido. Basta seguir os seguintes passos:

1. Instale o Java Development Kit (JDK).
2. Instale o Leiningen, uma ferramenta de construção de projetos em Clojure.
3. Crie um novo projeto com o comando ```lein new app nome-do-projeto```.
4. Abra o arquivo ```project.clj```, que contém as dependências do seu projeto.
5. Adicione as dependências desejadas e execute o comando ```lein deps``` para baixá-las.
6. Escreva seu código Clojure em arquivos com extensão ```.clj```.
7. Para executar o projeto, use o comando ```lein run```.

### Exemplo:

```Clojure
(ns meu-projeto.core)
(defn saudacao [nome]
  (println (str "Olá " nome)))

(saudacao "Usuário") ; imprime "Olá Usuário" no terminal
```

# Mergulhando mais fundo em iniciar um novo projeto

Ao iniciar um novo projeto em Clojure, é importante considerar algumas coisas:

- Clojure é uma linguagem que roda sobre a JVM, então é importante ter algum conhecimento de Java.
- A comunidade Clojure é muito ativa, então sempre há suporte e recursos disponíveis.
- Utilizar a funcionalidade pura do Clojure é uma das melhores maneiras de obter benefícios do uso dessa linguagem.
- Para projetos maiores e mais complexos, é recomendável utilizar um editor de código ou uma IDE específica para Clojure, como o Cursive ou o Calva.

# Veja também

- [Documentação oficial do Clojure](https://clojure.org/)
- [Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook)
- [Clojure for the Brave and True](https://www.braveclojure.com/)