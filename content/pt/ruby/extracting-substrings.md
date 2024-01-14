---
title:    "Ruby: Extraindo subtrings"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

#Por que

Extrair substrings é uma habilidade essencial para qualquer programador que deseja manipular e trabalhar com strings de maneira eficiente. Isso permite que você pegue partes específicas de uma string e as utilize para realizar determinadas tarefas, como filtrar dados ou validar informações inseridas pelo usuário.

#Como Fazer

```Ruby
# Exemplo de código em Ruby para extrair substrings
texto = "Olá, meu nome é João!"
# Podemos extrair o nome do texto acima
nome = texto[13..23]
# O resultado será "João"
```

No código acima, utilizamos o método `[]` para extrair uma substring a partir de uma string. Os números dentro dos colchetes representam as posições inicial e final em que desejamos extrair a substring. Lembre-se de que as posições em Ruby começam em 0, então a posição 13 é onde começa a palavra "João" no texto.

Também podemos utilizar outros métodos para extrair substrings, como `slice`, `slice!` e `sub`. Cada um desses métodos possui suas próprias propriedades e pode ser utilizado para diferentes finalidades. A prática é a melhor forma de compreender cada um deles.

#Mergulho Profundo

Há muitas coisas a serem consideradas ao extrair substrings em Ruby. Uma das mais importantes é o uso da sintaxe de intervalo (`..`) em comparação com a sintaxe de pontos (`...`).

Por exemplo, se utilizarmos `texto[13...23]` no código anterior, o resultado será "Joã", pois a posição final não é inclusa. Já se usarmos `texto[13..23]`, a posição final é inclusa e o resultado será "João".

Além disso, é importante entender como os métodos `slice` e `slice!` funcionam em diferentes situações, pois seus comportamentos podem variar dependendo dos argumentos passados.

É essencial ter um bom domínio dos diferentes métodos para extrair substrings em Ruby para que você possa utilizá-los de forma eficiente em seus projetos.

#Veja também
- [Documentação oficial do Ruby para extrair substrings](https://ruby-doc.org/core-3.0.2/String.html#method-i-slice)
- [Tutorial da Codeacademy sobre strings em Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-string-methods/cheatsheet)
- [Guia da MDN sobre manipulação de strings em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String)

Esperamos que este post tenha sido útil para você entender melhor como extrair substrings em Ruby. Pratique e explore os diferentes métodos disponíveis para dominar essa habilidade em sua programação!