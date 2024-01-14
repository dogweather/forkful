---
title:    "Javascript: Converter uma string para minúsculas"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Existem muitas razões pelas quais um programador pode querer converter uma string para letras minúsculas em Javascript. Uma delas é para facilitar nas operações de comparação de strings, já que letras maiúsculas e minúsculas são tratadas de forma diferente pela linguagem. Além disso, alguns dados podem estar em formato de maiúsculas e outros em minúsculas, então a conversão é necessária para padronização. Agora, vamos aprender como realizar essa conversão!

## Como fazer a conversão de uma string para letras minúsculas

A conversão de uma string para letras minúsculas em Javascript pode ser realizada de diversas maneiras, vamos ver algumas delas abaixo:

````Javascript
// Usando o método toLowerCase() em uma variável string
let texto = "Olá, MUNDO!"
console.log(texto.toLowerCase()); // saída: olá, mundo!

// Usando o método toLowerCase() diretamente em uma string
console.log("OLÁ, MUNDO!".toLowerCase()); // saída: olá, mundo!

// Usando a função toLowerCase() em uma variável string
let mensagem = "Conversão de String"
console.log(mensagem.toLowerCase()); // saída: conversão de string

// Usando a função toLowerCase() diretamente em uma string
console.log("CONVERSÃO DE STRING".toLowerCase()); // saída: conversão de string

// Loop para converter caracteres em minúsculas
let palavra = "ExEMPLO"
let novaPalavra = ""

for (let i = 0; i < palavra.length; i++) {
    let caractere = palavra.charAt(i).toLowerCase();
    novaPalavra += caractere
 }

console.log(novaPalavra); // saída: exemplo
````

Como podemos ver, existem diferentes maneiras de converter uma string para letras minúsculas em Javascript. Além dos métodos e funções mencionados acima, também é possível usar expressões regulares para realizar essa conversão de forma mais complexa. Agora, vamos aprofundar um pouco mais no assunto.

## Detalhando a conversão de uma string para letras minúsculas

Ao converter uma string para letras minúsculas, é importante levar em consideração o idioma utilizado. Em alguns idiomas, como o alemão, a conversão pode ser diferente, já que algumas letras minúsculas possuem caracteres especiais. Por isso, é importante testar a conversão em diferentes idiomas e garantir que ela esteja funcionando corretamente.

Além disso, é importante notar que ao converter uma string utilizando os métodos ou funções mencionados acima, uma nova string é retornada ao invés de alterar a string original. Ou seja, é necessário armazenar o resultado da conversão em uma nova variável ou substituir a string original pela nova string.

Por fim, é importante lembrar que ao realizar a conversão para letras minúsculas, acentos e outros caracteres especiais não são alterados, apenas as letras maiúsculas são convertidas. Portanto, é necessário ter isso em mente ao realizar operações que envolvam esses tipos de caracteres.

## Veja também

- [Documentação sobre o método toLowerCase()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Tutorial sobre expressões regulares em Javascript](https://www.w3schools.com/js/js_regexp.asp)
- [Mais informações sobre a conversão de caracteres em diferentes idiomas](https://www.unicode.org/index.html)