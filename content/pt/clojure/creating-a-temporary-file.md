---
title:                "Criando um arquivo temporário."
html_title:           "Clojure: Criando um arquivo temporário."
simple_title:         "Criando um arquivo temporário."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Criar um arquivo temporário significa criar um arquivo que será utilizado apenas temporariamente para armazenar informações necessárias em um programa. Programadores fazem isso para garantir que seus códigos sejam executados de maneira mais eficiente e para evitar sobrecarregar o sistema com arquivos desnecessários.

## Como fazer:

```Clojure
;; Em Clojure, podemos criar um arquivo temporário utilizando a função 'with-open'. 
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
  ;; Aqui dentro, podemos realizar qualquer operação com o arquivo, como escrever ou ler dados.
  (.write temp-file "Este é um exemplo de conteúdo do arquivo temporário.")
  ;; Assim que o bloco 'with-open' for concluído, o arquivo temporário será excluído automaticamente.
  )
```
Output:
Um arquivo de texto chamado 'tempXXXXXX.txt' será criado e armazenado na pasta temporária do sistema. O conteúdo do arquivo será "Este é um exemplo de conteúdo do arquivo temporário.".

## Mergulho Profundo:

Criar arquivos temporários é uma prática comum em programação e já existem diversas soluções implementadas em diferentes linguagens de programação. Em Clojure, a função 'with-open' é uma opção fácil e eficiente para criar e gerenciar arquivos temporários. No entanto, é importante lembrar que essa função só funciona para arquivos criados na pasta temporária do sistema e que a exclusão do arquivo é forçada quando o bloco 'with-open' é concluído.

## Veja também:

- Site oficial do Clojure: https://clojure.org/
- Documentação da função 'with-open': https://clojuredocs.org/clojure.core/with-open
- Outras formas de gerar arquivos temporários em Clojure: https://stackoverflow.com/questions/30330977/how-to-create-temporary-file-in-clojure