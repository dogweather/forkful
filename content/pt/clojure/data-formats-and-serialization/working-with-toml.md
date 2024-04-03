---
date: 2024-01-26 04:20:33.263413-07:00
description: "Trabalhar com TOML significa que voc\xEA est\xE1 manipulando dados em\
  \ um formato Minimal \"Tom's Obvious, Minimal Language\", popular para arquivos\
  \ de\u2026"
lastmod: '2024-03-13T22:44:46.219904-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com TOML significa que voc\xEA est\xE1 manipulando dados em um\
  \ formato Minimal \"Tom's Obvious, Minimal Language\", popular para arquivos de\
  \ configura\xE7\xE3o devido \xE0 sua facilidade de leitura."
title: Trabalhando com TOML
weight: 39
---

## O Quê & Porquê?
Trabalhar com TOML significa que você está manipulando dados em um formato Minimal "Tom's Obvious, Minimal Language", popular para arquivos de configuração devido à sua facilidade de leitura. Programadores o utilizam para gerenciamento de configuração direto e simples que funciona imediatamente com uma sintaxe amigável para humanos.

## Como Fazer:
Para trabalhar com TOML em Clojure, você precisa de uma biblioteca como `clj-toml`. Primeiro, adicione-a ao seu `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Depois, faça o parse de algum TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Example TOML'")

(def parsed-config (toml/parse-string config-str))

;; Pega o título do TOML parseado
(println (:title parsed-config)) ;; Saída: Exemplo TOML
```

Para gerar TOML:

```clojure
(def data {:title "Exemplo TOML"})

(println (toml/generate-string data))
;; Saída: title = "Exemplo TOML"
```

## Aprofundamento
O TOML foi criado em torno de 2013 por Tom Preston-Werner, co-fundador do GitHub, como uma alternativa mais simples ao YAML e JSON para arquivos de configuração. Visa clareza e pretende ser uma especificação que humanos podem ler sem ferramentas adicionais.

Enquanto JSON é frequentemente usado para APIs e aplicativos web, e YAML pode se tornar complexo com referências e habilidades de script, TOML se destaca com um foco em estruturas simples e baseadas em tabelas. Essa simplicidade o torna especialmente popular na comunidade Rust e outros ambientes de linguagem modernos.

Clojure, com seu foco em simplicidade e praticidade, combina bem com o TOML para configuração. `clj-toml` ou bibliotecas alternativas preenchem a lacuna. Eles traduzem os dados estáticos do TOML para o mundo dinâmico e funcional do Clojure.

## Veja Também
- Repositório GitHub do TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` no Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Documentação do Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Intro ao `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
