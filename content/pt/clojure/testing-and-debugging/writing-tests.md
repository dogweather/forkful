---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:07.422099-07:00
description: "Escrever testes em Clojure, assim como em outras linguagens de programa\xE7\
  \xE3o, envolve a cria\xE7\xE3o de c\xF3digo dedicado para verificar se sua base\
  \ de c\xF3digo\u2026"
lastmod: 2024-02-19 22:05:05.272642
model: gpt-4-0125-preview
summary: "Escrever testes em Clojure, assim como em outras linguagens de programa\xE7\
  \xE3o, envolve a cria\xE7\xE3o de c\xF3digo dedicado para verificar se sua base\
  \ de c\xF3digo\u2026"
title: Escrevendo testes
---

{{< edit_this_page >}}

## O Que & Por Quê?
Escrever testes em Clojure, assim como em outras linguagens de programação, envolve a criação de código dedicado para verificar se sua base de código principal funciona conforme o esperado. Isso ajuda a garantir a corretude, facilitar o refatoramento e melhorar a estabilidade do código.

## Como fazer:
Clojure, aproveitando o JVM, suporta vários frameworks de teste. No entanto, uma biblioteca integrada comumente usada é a `clojure.test`. Aqui está um exemplo simples:

```clojure
(ns exemplo.teste
  (:require [clojure.test :refer :all]
            [exemplo.nucleo :refer :all]))

(deftest teste-adicao
  (testing "Funcionalidade de adição"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Após executar este teste, você veria uma saída semelhante a:

```
Testando exemplo.teste

Executados 2 testes contendo 2 asserções.
0 falhas, 0 erros.
```

Para aqueles que buscam opções mais ricas em recursos, pode-se utilizar bibliotecas de terceiros como `Midje` ou `test.check`. Veja como você poderia usar o Midje para um teste semelhante:

Primeiro, adicione o Midje às dependências do seu project.clj:
```clojure
[midje "1.9.9"]
```

Então, seu teste com o Midje pode parecer assim:

```clojure
(ns exemplo.teste
  (:require [midje.sweet :refer :all]
            [exemplo.nucleo :refer :all]))

(fact "Testando adição"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Ao executar o teste através do Midje com `lein midje`, a saída exibiria algo parecido com:

```
Todas as verificações (2) foram bem-sucedidas.
```
