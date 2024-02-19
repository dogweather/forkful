---
aliases:
- /pt/clojure/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:32.805926-07:00
description: "Escrever um arquivo de texto em Clojure envolve criar ou modificar arquivos\
  \ para salvar dados fora de sua aplica\xE7\xE3o, possibilitando persist\xEAncia,\u2026"
lastmod: 2024-02-18 23:08:57.820742
model: gpt-4-0125-preview
summary: "Escrever um arquivo de texto em Clojure envolve criar ou modificar arquivos\
  \ para salvar dados fora de sua aplica\xE7\xE3o, possibilitando persist\xEAncia,\u2026"
title: Escrevendo um arquivo de texto
---

{{< edit_this_page >}}

## O Que & Por Que?

Escrever um arquivo de texto em Clojure envolve criar ou modificar arquivos para salvar dados fora de sua aplicação, possibilitando persistência, configuração, registro (logging) ou comunicação entre processos. Programadores realizam essa tarefa para externalizar o estado da aplicação, configurações ou compartilhar informações entre diferentes partes de um programa ou entre programas distintos.

## Como fazer:

### Escrevendo texto em um arquivo usando funções nativas do Clojure

A função `spit` é a maneira mais simples de escrever texto em um arquivo em Clojure. Ela recebe dois argumentos: o caminho do arquivo e a string a ser escrita. Se o arquivo não existir, `spit` irá criá-lo. Se já existir, `spit` irá sobrescrevê-lo.

```clojure
(spit "example.txt" "Olá, mundo!")
```

Para acrescentar texto a um arquivo existente, você pode usar a função `spit` com a opção `:append`.

```clojure
(spit "example.txt" "\nVamos adicionar esta nova linha." :append true)
```

Após executar esses trechos de código, "example.txt" conterá:

```
Olá, mundo!
Vamos adicionar esta nova linha.
```

### Usando bibliotecas de terceiros

Embora as capacidades nativas do Clojure frequentemente sejam suficientes, a comunidade desenvolveu bibliotecas robustas para tarefas mais complexas ou específicas. Para entrada/saída (I/O) de arquivos, uma biblioteca popular é `clojure.java.io`, que fornece uma abordagem mais semelhante ao Java para o manuseio de arquivos.

Para usar `clojure.java.io` para escrever em um arquivo, primeiro você precisa importá-la:

```clojure
(require '[clojure.java.io :as io])
```

Então, você pode usar a função `writer` para obter um objeto escritor, e a função `spit` (ou outras, como `print`, `println`) para escrever no arquivo:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "Isso foi escrito usando clojure.java.io"))
```

Isso criará (ou sobrescreverá, se já existir) "example_with_io.txt" com o texto:

```
Isso foi escrito usando clojure.java.io
```

Lembre-se: `with-open` garante que o arquivo seja fechado devidamente após a escrita, evitando possíveis vazamentos de recursos.
