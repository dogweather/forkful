---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---

## O que & Por quê?

Pesquisar e substituir texto refere-se a localizar um texto específico (string) e trocá-lo. É uma operação comum para qualquer programador, seja para modificar código, corrigir erros ou processar dados.

---

## Como fazer:

No Bash, utilizamos `sed` para pesquisar e substituir texto. Aqui está um exemplo simples:

```Bash
echo "Olá, mundo!" | sed 's/mundo!/Programação Bash/g'
```

O seguinte é impresso na tela: 

```Bash
Olá, Programação Bash!
```

Nesse exemplo, `sed 's/mundo!/Programação Bash/g'` localiza 'mundo!' e substitui por 'Programação Bash'.

---

## Mergulho profundo:

1. **Contexto histórico:** `sed` é uma ferramenta de edição de fluxos que tem sido uma parte essencial do Unix desde os primórdios do sistema operacional.
2. **Alternativas:** Existem várias alternativas ao `sed`, tais como `awk`, `perl`, `python`, etc. Todos tem suas próprias vantagens dependendo do fluxo de trabalho.
3. **Detalhes de implementação:** `sed 's/pesquisa/substituição/g'` usa expressões regulares para pesquisar o texto. A letra 'g' no final é para substituir globalmente todos as ocorrências no texto.

---

## Veja também:

* [Tutorial sed em Português](http://aurelio.net/sed/)
* [Manipulação de strings no Bash](https://www.cyberciti.biz/faq/unix-linux-bash-remove-string/)
* [Referência de expressões regulares para sed](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)