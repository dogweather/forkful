---
title:                "Clojure: Busca e substituição de texto"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por que usar o Clojure para pesquisar e substituir texto?

Ao trabalhar com programação, muitas vezes precisamos manipular grandes quantidades de texto. Uma das tarefas mais comuns é a de procurar e substituir certas palavras ou frases em um texto. Nesses casos, o uso do Clojure pode facilitar muito o processo, graças à sua sintaxe concisa e poderosa.

# Como fazer isso?

Primeiramente, importe a biblioteca `clojure.string`, que nos permite trabalhar com strings de forma eficiente. Na maioria dos casos, utilizaremos a função `replace`, que recebe três parâmetros: a string original, a expressão regular para a qual desejamos substituir e o resultado da substituição. Por exemplo:

```Clojure
(require '[clojure.string :as str])
(str/replace "Olá, meu nome é João!" #"João" "Maria")
```

Nesse caso, o resultado será: "Olá, meu nome é Maria!".

Podemos também usar uma função de substituição customizada, utilizando a função `clojure.string/replace-with`. Se quisermos, por exemplo, substituir todas as vogais de uma palavra por um asterisco, podemos fazer o seguinte:

```Clojure
(require '[clojure.string :as str])
(str/replace-with #"[aeiou]" "programação" (fn [match] "*"))
```

O resultado será: "pr*g*r*m***".

# Aprofundando-se

Além das funções mencionadas acima, o Clojure também nos oferece diversas outras possibilidades para trabalhar com strings. Podemos, por exemplo, utilizar a função `str/split` para dividir uma string em um vetor, utilizando um determinado caractere como separador. Ou a função `str/trim` para remover espaços vazios no início e no final de uma string.

Outra técnica muito útil é a utilização de `regex` para substituir textos em locais específicos. A função `str/replace-first` aceita uma expressão regular e substitui apenas a primeira ocorrência encontrada, enquanto `str/replace-last` substitui a última ocorrência.

# Veja também

- Documentação oficial da biblioteca string do Clojure: https://clojure.github.io/clojure/clojure.string-api.html
- Como usar regex em Clojure: https://www.mkyong.com/clojure/clojure-regex/
- Outros exemplos de como manipular strings em Clojure: https://medium.com/better-programming/5-tricks-to-manipulate-strings-in-clojure-like-a-pro-8f9daf544e26

Esperamos que esse artigo tenha mostrado algumas das formas práticas de como o Clojure pode nos ajudar a realizar tarefas de manipulação de texto de forma eficiente. Com um pouco de prática, você estará dominando essas técnicas e poupando tempo em seus projetos de programação.