---
title:                "Clojure: Criando um arquivo temporário"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em Clojure?

Criar um arquivo temporário é uma prática comum em programação, especialmente em linguagens como Clojure. Isso é útil para realizar operações que requerem um espaço temporário de armazenamento, como gerar arquivos de logs ou armazenar dados temporários durante a execução de um programa.

## Como fazer

Para criar um arquivo temporário em Clojure, podemos usar a função `with-open` combinada com `newFileOutputStream` da biblioteca `clojure.java.io`.

```
Clojure
(with-open [output-stream (newFileOutputStream "arquivo_temporario.txt")]
  (write output-stream "Este é um exemplo de texto a ser escrito no arquivo temporário"))
```

## Detalhando a criação de arquivos temporários

Ao criar um arquivo temporário em Clojure, é importante lembrar de alguns pontos importantes:

- O arquivo temporário será criado em um diretório específico definido pelo sistema operacional.
- O nome do arquivo gerado é garantido que seja único, evitando conflitos com outros arquivos gerados pelo programa.
- O arquivo será automaticamente excluído após o término da execução do programa.

# Veja também

- [Função `with-open` em Clojure](https://clojuredocs.org/clojure.core/with-open)
- [Biblioteca `clojure.java.io`](https://clojuredocs.org/clojure.java.io)
- [Documentação oficial sobre arquivos temporários em Java](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)