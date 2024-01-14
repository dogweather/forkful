---
title:    "Clojure: Extraindo Substrings"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma habilidade importante para qualquer programador Clojure. Isso permite que você trabalhe com partes específicas de uma string em vez de lidar com a string inteira. Isso pode ser útil em tarefas como manipulação de dados, validação de entrada e formatação de texto.

## Como Fazer

Extrair substrings em Clojure é bastante simples. Use a função `subs` e forneça a string original, o índice inicial e o índice final desejados. O índice final é opcional e, se omitido, a substring irá até o fim da string original.

Exemplo: 
```Clojure
(subs "Olá mundo!" 4 9)
```

Isso produzirá a saída "mundo", pois estamos especificando o índice inicial como 4 e o índice final como 9, que é o caractere "o" em "mundo".

Você também pode usar números negativos para especificar o índice a partir do final da string. Por exemplo, se quisermos extrair "olá" da string "Olá mundo!", podemos usar:
```Clojure
(subs "Olá mundo!" 0 -6)
```

Isso produzirá a saída "olá", pois estamos especificando o índice inicial como 0 e o índice final como -6, que é seis caracteres antes do final da string original.

## Mergulho Profundo

Além do `subs`, Clojure também tem outras funções úteis para extrair substrings, como `subseq`, que retorna uma sequência de caracteres em vez de uma string, e `substring`, que permite que você especifique o índice inicial e a quantidade de caracteres para extrair.

Também é importante lembrar que Clojure usa índices baseados em zero, o que significa que o primeiro caractere de uma string é representado pelo índice 0, o segundo pelo índice 1 e assim por diante.

Se você quiser saber mais sobre a manipulação de strings em Clojure, recomendamos a leitura da documentação oficial e a prática desses exemplos em seu próprio ambiente de desenvolvimento.

## Veja Também

- Documentação oficial Clojure sobre `subs`: https://clojuredocs.org/clojure.core/subs
- Documentação oficial Clojure sobre `subseq`: https://clojuredocs.org/clojure.core/subseq
- Documentação oficial Clojure sobre `substring`: https://clojuredocs.org/clojure.core/substring