---
title:                "Clojure: Escrevendo um arquivo de texto"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto pode ser uma tarefa simples, mas é extremamente útil quando se trata de armazenar informações ou configurar um programa. Além disso, ao usar uma linguagem funcional como Clojure, a escrita de arquivos de texto pode ser feita de forma elegante e concisa.

## Como fazer

Para escrever um arquivo de texto em Clojure, podemos usar a função "spit", que recebe como parâmetros o caminho do arquivo e o conteúdo que será escrito. Por exemplo:

```Clojure
(spit "arquivo.txt" "Olá, mundo!")
```

Com isso, criamos um arquivo chamado "arquivo.txt" que contém o texto "Olá, mundo!".

Podemos também usar o operador ">>" para adicionar conteúdo a um arquivo já existente, como mostrado no exemplo abaixo:

```Clojure
(>> "arquivo.txt" "Isso é um texto adicionado ao arquivo!")
```

Outra opção é usar a função "with-open" para garantir que o arquivo seja fechado após a escrita. Veja o exemplo:

```Clojure
(with-open [arq (writer "arquivo.txt")]
  (.write arq "Este texto é escrito dentro do bloco 'with-open'"))
```

## Mergulho profundo

Quando escrevemos um arquivo de texto, podemos especificar o tipo de codificação que será utilizado, se o arquivo será criado se não existir, entre outras configurações. Para isso, podemos passar mapas como argumentos para a função "spit".

Por exemplo, para escrever um arquivo usando a codificação UTF-8 e criá-lo caso ele não exista, podemos usar o seguinte código:

```Clojure
(spit "arquivo.txt" "Conteúdo do arquivo" {:encoding "UTF-8" :append false})
```

Também é possível usar a função "slurp" para ler o conteúdo de um arquivo de texto, e a função "read-string" para transformar esse conteúdo em uma estrutura de dados do Clojure. Veja o exemplo:

```Clojure
(def texto (slurp "arquivo.txt"))
(def dados (read-string texto))
```

## Veja também

- [Documentação oficial do Clojure sobre escrita de arquivos](https://clojuredocs.org/clojure.core/spit)
- [Tutorial de Clojure para iniciantes em português](https://www.tundebabzy.com/pt/tutorials/clojure/collections)