---
title:                "Pesquisando e substituindo texto"
aliases:
- /pt/clojure/searching-and-replacing-text.md
date:                  2024-01-20T17:57:20.774285-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que É & Por Que?
Pesquisar e substituir texto é a arte de encontrar certas palavras ou padrões e trocá-los por algo novo. Programadores fazem isso para corrigir erros, atualizar dados ou refatorar código de maneira eficiente.

## Como Fazer:
```Clojure
(defn substituir-texto
  "Troca todas as ocorrências de 'procurado' por 'substituto' no 'texto'."
  [texto procurado substituto]
  (clojure.string/replace texto procurado substituto))

;; Exemplo de uso:
(substituir-texto "Olá, mundo!" "mundo" "Clojure")
;; Saída esperada: "Olá, Clojure!"
```

## Mergulho Profundo
Substituir texto é uma operação fundamental em muitas áreas da computação, com raízes nos primórdios da edição de texto em terminais Unix. Alternativas de implementação podem envolver expressões regulares para padrões complexos ou funções de alto nível da linguagem. Em Clojure, a simplicidade é rei – a standard library já vem com o necessário, mas você sempre pode estender com pacotes poderosos como "clojure.spec" para validações mais complexas.

## Veja Também
- ClojureDocs para exemplos da comunidade: [https://clojuredocs.org/](https://clojuredocs.org/)
- Guia da linguagem Clojure: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- Documentação da função `clojure.string/replace`: [https://clojuredocs.org/clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
