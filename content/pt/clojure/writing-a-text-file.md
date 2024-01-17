---
title:                "Escribir un archivo de texto"
html_title:           "Clojure: Escribir un archivo de texto"
simple_title:         "Escribir un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O quê e por quê?

Escrever um arquivo de texto é um processo de criar um documento de texto que pode ser lido e manipulado por computadores. Os programadores geralmente escrevem arquivos de texto para armazenar e compartilhar informações importantes, como configurações, logs ou dados de entrada.

## Como fazer:

```Clojure
(with-open [file (io/writer "arquivo.txt")]  ; Abre o arquivo de texto como escritor
  (.write file "Olá, mundo!")                ; Escreve "Olá, mundo!" no arquivo
  (.close file))                             ; Fecha o arquivo
```

O código acima utiliza a função `with-open`, que garante que o arquivo seja fechado após a escrita. Também é possível utilizar a função `spit` para escrever em um arquivo de texto:

```Clojure
(spit "arquivo.txt" "Olá, mundo!")
```

Ambos os exemplos produzem o arquivo "arquivo.txt" com o conteúdo "Olá, mundo!".

## Profundando:

Escrever arquivos de texto tem sido uma parte fundamental da programação desde o início do desenvolvimento de software. Antigamente, era necessário utilizar comandos específicos do sistema operacional para escrever em arquivos de texto, mas com o avanço das linguagens de programação, surgiram funções e bibliotecas para facilitar esse processo.

Além da função `spit` mencionada anteriormente, também é possível utilizar a função `slurp` para ler o conteúdo de um arquivo de texto para uma variável:

```Clojure
(def conteudo (slurp "arquivo.txt")) ; Lê o conteúdo do arquivo "arquivo.txt" para a variável "conteudo"
```

Outra alternativa para escrever arquivos de texto é utilizar bibliotecas externas, como a `clomp` ou a `unilog`. Essas bibliotecas oferecem funcionalidades adicionais, como escrever em arquivos de texto em outros formatos, como CSV ou JSON.

## Veja também:

- [Documentação oficial sobre arquivos I/O em Clojure](https://clojure.org/reference/io)
- [Tutorial sobre manipulação de arquivos em Clojure](https://practicalli.github.io/clojure/io.html)
- [Exemplos de uso da função `spit`](https://purelyfunctional.tv/guide/working-with-files/)