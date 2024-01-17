---
title:                "Escrevendo no erro padrão"
html_title:           "Elm: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

O que e Por Que?

Escrever para o erro padrão é uma técnica usada pelos programadores para exibir mensagens de erro ou informações de depuração durante a execução de um programa. Isso permite que os desenvolvedores vejam o que está acontecendo no código em tempo real e facilite a identificação e correção de erros.

Como Fazer:

Para escrever para o erro padrão em Elm, usamos a função "Debug.log" que aceita uma string como primeiro argumento e um valor como segundo argumento. Aqui está um exemplo:

Elm ...

[1, 2, 3] 
    |> Debug.log "Lista inicial" 
    |> List.map (* 2) 
    |> Debug.log "Lista multiplicada por 2"

O resultado seria:

Lista inicial: [1, 2, 3] 
Lista multiplicada por 2: [2, 4, 6]

Neste exemplo, usamos o "Debug.log" para exibir a lista inicial e a lista multiplicada por 2 para ajudar a entender como a função "List.map" funciona.

Mergulho Profundo:

A técnica de escrever para o erro padrão tem suas raízes nos primórdios da programação, quando os desenvolvedores tinham que usar dispositivos de saída externos para exibir informações úteis durante a execução do código. Hoje em dia, existem ferramentas mais avançadas para depuração, como depuradores e registradores, mas escrever para o erro padrão ainda é amplamente utilizado por sua simplicidade e facilidade de implementação em várias linguagens de programação.

Veja Também:

Para saber mais sobre como escrever para o erro padrão em Elm, confira a documentação oficial do idioma em seu site oficial. Além disso, você pode explorar outras técnicas de depuração e ferramentas disponíveis para melhorar sua eficiência como desenvolvedor.