---
aliases:
- /pt/clojure/using-an-interactive-shell-repl/
date: 2024-01-26 04:13:04.143132-07:00
description: "REPL, ou Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3o, \xE9 um ambiente\
  \ de programa\xE7\xE3o para testar dinamicamente c\xF3digo Clojure peda\xE7o por\
  \ peda\xE7o. Programadores o\u2026"
lastmod: 2024-02-18 23:08:57.804305
model: gpt-4-0125-preview
summary: "REPL, ou Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3o, \xE9 um ambiente\
  \ de programa\xE7\xE3o para testar dinamicamente c\xF3digo Clojure peda\xE7o por\
  \ peda\xE7o. Programadores o\u2026"
title: Usando um shell interativo (REPL)
---

{{< edit_this_page >}}

## O Que & Por Quê?
REPL, ou Loop de Leitura-Avaliação-Impressão, é um ambiente de programação para testar dinamicamente código Clojure pedaço por pedaço. Programadores o utilizam para obter feedback imediato, desenvolvimento iterativo e experimentações rápidas sem o ônus de compilar ou configurar um ambiente de projeto completo.

## Como:
Comece lançando o REPL:

```Clojure
user=> (println "Olá, REPL!")
Olá, REPL!
nil
```

Defina uma função e experimente:
```Clojure
user=> (defn greet [name] (str "Olá, " name "!"))
#'user/greet
user=> (greet "Programador Clojure")
"Olá, Programador Clojure!"
```

Experimente com estruturas de dados:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Mergulho Profundo
O REPL é chave para a filosofia de desenvolvimento interativo da família Lisp, e o Clojure, um dialeto moderno de Lisp, faz grande uso desta ferramenta. Ele remonta ao primeiro REPL Lisp no final dos anos 1950. Alternativas em outras linguagens incluem o interpretador Python e o console do Node.js, mas o REPL do Clojure tem status de primeira classe e é integral ao fluxo de trabalho.

Uma sessão de REPL Clojure pode ser integrada em vários ambientes como a linha de comando, IDEs (tal como IntelliJ com Cursive, ou Emacs com CIDER), ou ferramentas baseadas em navegador como o Nightcode. Em um sentido mais profundo, o REPL empodera o desenvolvedor a manipular as construções da linguagem em tempo de execução e manter estados através de várias transformações, levando frequentemente a programação exploratória e um código mais robusto.

A funcionalidade do REPL se destaca com ferramentas como `lein repl` ou `clj`, que permitem gerenciamento de dependências, vários plugins, e customizações específicas do projeto, levando a um processo de desenvolvimento mais produtivo e flexível.

## Veja Também
- O guia oficial do site Clojure sobre o REPL: https://clojure.org/guides/repl/introduction
- A palestra de Rich Hickey sobre desenvolvimento guiado por REPL: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Clojure Prático: usando o REPL para desenvolvimento iterativo: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
