---
date: 2024-01-20 17:57:20.774285-07:00
description: "Como Fazer: Substituir texto \xE9 uma opera\xE7\xE3o fundamental em\
  \ muitas \xE1reas da computa\xE7\xE3o, com ra\xEDzes nos prim\xF3rdios da edi\xE7\
  \xE3o de texto em terminais Unix.\u2026"
lastmod: '2024-04-05T21:53:46.504443-06:00'
model: gpt-4-1106-preview
summary: "Substituir texto \xE9 uma opera\xE7\xE3o fundamental em muitas \xE1reas\
  \ da computa\xE7\xE3o, com ra\xEDzes nos prim\xF3rdios da edi\xE7\xE3o de texto\
  \ em terminais Unix."
title: Pesquisando e substituindo texto
weight: 10
---

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
