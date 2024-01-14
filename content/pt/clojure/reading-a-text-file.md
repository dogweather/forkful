---
title:    "Clojure: Lendo um arquivo de texto"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Clojure?

Ler arquivos de texto é uma tarefa comum em muitos projetos de programação. Pode ser necessário para extrair dados, manipular informações ou até mesmo para verificar a integridade do arquivo. Em Clojure, essa tarefa pode ser realizada de forma rápida e eficiente com algumas simples linhas de código. Neste artigo, vamos explorar como ler um arquivo de texto em Clojure e aprofundar um pouco mais nos conceitos por trás dessa operação.

## Como fazer

A primeira etapa para ler um arquivo de texto em Clojure é importar a biblioteca "java.io". Isso pode ser feito com o uso da macro "require", como mostrado no exemplo abaixo:

```Clojure 
(require '[clojure.java.io :as io])
```

A seguir, utilizamos a função "slurp" para ler o conteúdo do arquivo de texto. Essa função aceita o caminho do arquivo como argumento e retorna uma string com todo o conteúdo. Por exemplo, se queremos ler o conteúdo de um arquivo chamado "nomes.txt", nosso código seria o seguinte:

```Clojure
(slurp "nomes.txt")
```

Isso irá retornar uma string contendo todos os nomes presentes no arquivo. Podemos então usar essa string de diversas maneiras, como imprimir na tela, manipular os dados ou até mesmo salvar em outro arquivo.

## Profundidade

A função "slurp" é uma das formas mais simples de ler um arquivo de texto em Clojure, porém ela não é a única. Podemos também utilizar as funções "reader" e "read-line" para ler o conteúdo do arquivo linha por linha. Dessa forma, podemos manipular cada linha separadamente, o que é útil em caso de arquivos grandes.

Outra opção é utilizar a biblioteca "clojure.data.csv", que permite ler e escrever arquivos CSV (Comma-Separated Values) com facilidade. Essa é uma opção interessante caso o arquivo de texto seja estruturado em formato de tabela.

No entanto, é importante lembrar que ao ler um arquivo de texto, devemos sempre ter cuidado com a codificação utilizada. Em alguns casos, pode ser necessário especificar o tipo de codificação durante a leitura para garantir que os caracteres sejam interpretados corretamente.

## Veja também

Aqui estão alguns recursos adicionais que podem ser úteis na leitura de arquivos de texto em Clojure:

- [Documentação oficial de leitura e escrita de arquivos em Clojure](https://clojure.org/reference/reading)
- [Exemplos de leitura de arquivos com a biblioteca "clojure.data.csv"](https://github.com/clojure/data.csv/blob/master/src/main/clojure/clojure/data/csv/examples.clj)
- [Tutorial de Clojure para iniciantes](https://clojure.org/guides/getting_started)