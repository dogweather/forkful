---
title:                "Clojure: Verificando se um diretório existe"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Existem várias razões pelas quais pode ser importante para um programador verificar se um diretório existe:

- Verificar se um caminho de diretório especificado pelo usuário é válido antes de realizar operações nele.
- Garantir que um diretório necessário para o funcionamento do programa já está criado antes de prosseguir.
- Detectar erros ou problemas com o sistema de arquivos antes que eles causem problemas maiores em tempo de execução.

## Como fazer isso em Clojure

Para verificar se um diretório existe em Clojure, usamos a função `clojure.java.io/file`. Essa função retorna um objeto `java.io.File` que representa o diretório especificado por um caminho de diretório.

```Clojure
(def my-directory (clojure.java.io/file "/caminho/do/diretorio"))

(println (.exists my-directory))

; Output:
; true
```

Podemos também passar um objeto `java.io.File` diretamente para a função `clojure.java.io/file`:

```Clojure
(def my-other-directory (clojure.java.io/file my-directory))

(println (.exists my-other-directory))

; Output:
; true
```

Note que, se o diretório não existir, a função `exists` retornará `false`.

## Mergulho Profundo

Ao verificar se um diretório existe em Clojure, é importante estar ciente de que existem algumas diferenças entre os sistemas operacionais. Por exemplo, em sistemas Windows, o caminho do diretório pode incluir caracteres especiais que precisam ser substituídos antes de serem passados para a função `clojure.java.io/file`. Por outro lado, em sistemas Unix, o caminho do diretório deve estar em um formato específico para ser lido corretamente.

Outro fator a ser considerado é que a verificação de um diretório não garantirá que ele exista durante toda a duração do programa. Outros processos ou até mesmo ações do usuário podem alterar ou excluir o diretório após a verificação.

## Veja também

- [Documentação oficial do clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- [Guia de referência rápida do Clojure](https://clojure.org/guides/learn/quickref)