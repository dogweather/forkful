---
title:                "Escrevendo um arquivo de texto"
html_title:           "Clojure: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Existem várias razões pelas quais alguém pode querer escrever um arquivo de texto em Clojure. Algumas dessas razões podem incluir armazenar informações de configuração, gerar relatórios ou criar dados para serem usados em outros programas.

## Como fazer

Agora que entendemos por que escrever um arquivo de texto pode ser útil, vamos dar uma olhada em como podemos fazer isso em Clojure. Primeiro, vamos criar um novo arquivo de texto chamado "exemplo.txt" usando a função `spit`:

```Clojure
(spit "exemplo.txt" "Este é um exemplo de texto em um arquivo criado usando Clojure.")
```

Isso criará um arquivo chamado "exemplo.txt" que contém o texto especificado. Podemos então usar a função `slurp` para ler o conteúdo do arquivo e exibi-lo no console:

```Clojure
(slurp "exemplo.txt")
```

Isso resultará na saída:

```
Este é um exemplo de texto em um arquivo criado usando Clojure.
```

Também podemos adicionar variáveis ou dados a serem escritos no arquivo, basta usar a sintaxe de interpolação de strings com a função `format`:

```Clojure
(def nome "João")
(def idade 25)

(spit "dados.txt" (format "Nome: %s, Idade: %d" nome idade))
```

Isso criará um arquivo chamado "dados.txt" com o seguinte conteúdo:

```
Nome: João, Idade: 25
```

## Profundidade

Agora que vimos como criar e ler arquivos de texto em Clojure, podemos nos aprofundar um pouco mais no assunto. Uma coisa importante a se notar é que, ao usar a função `spit`, por padrão, o conteúdo do arquivo é sobrescrito caso ele já exista. Se quisermos adicionar texto ao final do arquivo, podemos usar a opção `:append true`. Além disso, podemos especificar o formato do arquivo a ser criado usando a opção `:encoding`, por exemplo, `:encoding "UTF-8"`.

Além disso, também é possível criar diretórios usando a função `file-seq` e especificando o caminho desejado, por exemplo:

```Clojure
(file-seq "caminho/do/diretorio")
```

Isso retornará uma lista de todos os arquivos e diretórios no caminho especificado.

## Veja também

- [Documentação da função `spit`](https://clojuredocs.org/clojure.core/spit)
- [Documentação da função `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [Documentação da função `format`](https://clojuredocs.org/clojure.core/format)
- [Documentação da função `file-seq`](https://clojuredocs.org/clojure.java.io/file-seq)