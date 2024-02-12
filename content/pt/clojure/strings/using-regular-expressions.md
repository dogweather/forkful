---
title:                "Usando expressões regulares"
aliases:
- /pt/clojure/using-regular-expressions.md
date:                  2024-02-03T19:16:29.378046-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Expressões regulares, uma ferramenta poderosa para correspondência de padrões e manipulação de dados, são essenciais em tarefas de processamento de texto como validar entrada, pesquisar e substituir texto. Programadores as usam extensivamente para lidar com análise de strings complexas e tarefas de validação de dados de forma eficiente e sucinta.

## Como:

Clojure, fiel às suas raízes na família Lisp, oferece um rico conjunto de funções que se integram perfeitamente com as capacidades de expressão regular do Java. Veja como você pode aproveitá-las:

### Correspondência Básica
Para verificar se uma string corresponde a um padrão, use `re-matches`. Ele retorna a correspondência completa se for bem-sucedido ou `nil` caso contrário.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Pesquisando por Padrões
Para encontrar a primeira ocorrência de um padrão, `re-find` é sua função de escolha:

```clojure
(re-find #"\d+" "Pedido 123")  ;=> "123"
```

### Capturando Grupos
Use `re-find` junto com parênteses em seu padrão para capturar grupos:

```clojure
(let [[_ área código] (re-find #"(1)?(\d{3})" "Telefone: 123-4567")]
  (println "Código de Área:" área "Código:" código))
;; Saída: Código de Área: nil Código: 123
```

### Pesquisa Global (Encontrar Todas as Correspondências)
Clojure não tem uma pesquisa global embutida como algumas linguagens. Em vez disso, use `re-seq` para obter uma sequência preguiçosa de todas as correspondências:

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### Dividindo Strings
Para dividir uma string com base em um padrão, use `clojure.string/split`:

```clojure
(clojure.string/split "João,Doe,30" #",")  ;=> ["João" "Doe" "30"]
```

### Substituição
Substitua partes de uma string que correspondem a um padrão com `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "AAAA")  ;=> "AAAA-04-01"
```

### Bibliotecas de Terceiros
Embora o suporte interno de Clojure seja suficiente na maioria dos casos, para cenários mais complexos, considere usar bibliotecas como `clojure.spec` para validação robusta de dados e `reagent` para manipulação reativa do DOM em aplicações web com roteamento baseado em regex e validação de entrada.

```clojure
;; Exemplo usando clojure.spec para validar um email
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "teste@example.com")  ;=> true
```

Lembre-se, embora as expressões regulares sejam poderosas, elas também podem tornar o código difícil de ler e manter. Use-as com parcimônia e sempre considere funções de manipulação de string mais simples quando possível.
