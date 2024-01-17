---
title:                "Unindo strings"
html_title:           "Gleam: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê? 
Concatenar strings é quando você combina duas ou mais strings para criar uma nova string. Os programadores geralmente fazem isso quando precisam criar frases ou mensagens dinâmicas em seus programas. 

## Como: 
```
Gleam
"Olá, " ++ "mundo!" // Saída: "Olá, mundo!"
```
```
let nome_da_pessoa = "Maria"
let mensagem = "Bem-vindo, " ++ nome_da_pessoa 
```
```
"O código acima criará a string 'Bem-vindo, Maria' que pode ser usada para saudar usuários individualmente."
```

## Deep Dive: 
A concatenação de strings é uma técnica comum em programação e é amplamente utilizada em uma variedade de linguagens de programação. Além do operador "++" usado no Gleam, outras linguagens possuem suas próprias maneiras de concatenar strings, como o "+" no JavaScript e o "&" no Visual Basic. A concatenação também pode ser feita com funções específicas, como "concat" no Python. Em termos de implementação, a concatenação de strings geralmente envolve a alocação de espaço suficiente na memória para a nova string resultante e, em seguida, copiar as strings originais para esse espaço alocado. 

## See Also: 
Para mais informações sobre concatenar strings em Gleam, consulte a documentação oficial em [https://gleam.run/learn/docs/the-gleam-book#strings](https://gleam.run/learn/docs/the-gleam-book#strings). Para aprender mais sobre as diferentes formas de concatenar strings em outras linguagens, confira [https://www.tutorialspoint.com/concatenating-strings-in-different-languages](https://www.tutorialspoint.com/concatenating-strings-in-different-languages).