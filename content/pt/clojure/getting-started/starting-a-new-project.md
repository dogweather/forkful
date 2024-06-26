---
date: 2024-01-20 18:03:32.289010-07:00
description: "How to: Para iniciar um novo projeto em Clojure, usamos a ferramenta\
  \ `Leiningen`. Certifique-se de que est\xE1 instalada e ent\xE3o, execute."
lastmod: '2024-03-13T22:44:46.197413-06:00'
model: gpt-4-1106-preview
summary: Para iniciar um novo projeto em Clojure, usamos a ferramenta `Leiningen`.
title: Iniciando um novo projeto
weight: 1
---

## How to:
Para iniciar um novo projeto em Clojure, usamos a ferramenta `Leiningen`. Certifique-se de que está instalada e então, execute:

```Clojure
lein new app seu-projeto
```

Isso cria uma nova pasta chamada `seu-projeto` com a estrutura inicial do projeto. Um `project.clj` será criado, ele é o coração das configurações do seu projeto. Aqui está um exemplo básico de `project.clj`:

```Clojure
(defproject seu-projeto "0.1.0-SNAPSHOT"
  :description "Descrição do seu maravilhoso projeto"
  :url "http://seuprojeto.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot seu-projeto.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
```

Para rodar o projeto:

```Clojure
lein run
```

E para gerar um executável JAR:

```Clojure
lein uberjar
```

O resultado será dois arquivos `.jar` na pasta `target`.

## Deep Dive
Clojure é uma linguagem moderna que emana do Lisp, focando em programação funcional e imutabilidade. O `Leiningen` é o gerenciador de projetos e dependências mais popular para Clojure desde que foi criado por Phil Hagelberg em 2010. Derivado do nome de um nobre finlandês, Leiningen simplificou o início e a gestão de projetos Clojure.

Antes do Leiningen, os programadores Clojure montavam a estrutura do projeto manualmente, o que era propenso a erros e tomava tempo. Hoje, além do Leiningen, existem outras alternativas como `Boot` e o mais recente `Clojure CLI Tools`, mas Leiningen continua sendo o ponto de partida padrão para muitos desenvolvedores por sua simplicidade e comunidade estabelecida.

Implementar um projeto Clojure com o Leiningen envolve entender sobre artefatos, grupos e versões, essenciais para o gerenciamento de dependências. A estrutura padrão de diretórios e a gestão automática de classpaths ajudam a focar no que importa: codar.

## See Also
- [Leiningen's Official Website](https://leiningen.org/)
- [Clojure's Official Getting Started Guide](https://clojure.org/guides/getting_started)
- [ClojureDocs, uma excelente fonte de dados de funções da linguagem](https://clojuredocs.org/)
- [The Clojure Style Guide](https://guide.clojure.style/)
