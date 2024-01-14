---
title:    "Javascript: Procurando e Substituindo Texto"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Por que procurar e substituir texto em Javascript?

Ao trabalhar com programação, é comum encontrar situações em que precisamos fazer alterações em um determinado texto. Em vez de fazê-las manualmente, podemos utilizar o recurso de busca e substituição de texto em Javascript para agilizar o processo e evitar erros humanos. 

## Como fazer a busca e substituição de texto em Javascript

Para realizar a busca e substituição de texto em Javascript, podemos utilizar o método `replace()` junto com expressões regulares (regex). A sintaxe básica é a seguinte:

```Javascript 
var novoTexto = textoOriginal.replace(regex, substituicao); 
```

Vamos analisar alguns exemplos para entender melhor como funciona:

**Exemplo 1**: Substituindo uma palavra em um texto completo

Considere o texto "Eu amo programar em Javascript!" e queremos substituir a palavra "amo" por "gosto". Nesse caso, podemos utilizar o seguinte código:

```Javascript
var textoOriginal = "Eu amo programar em Javascript!";
var novoTexto = textoOriginal.replace(/amo/, "gosto");
console.log(novoTexto);

// Output: Eu gosto programar em Javascript!
```

Note que a palavra "amo" está representada pela regex `/amo/` e é substituída pela palavra "gosto". Ao executar o código, o novo texto, com a alteração realizada, será impresso no console.

**Exemplo 2**: Substituindo todas as ocorrências de uma palavra em um texto

Caso queiramos substituir todas as ocorrências de uma palavra em um texto, podemos adicionar a flag `g` (de global) à regex, indicando que queremos que a substituição seja feita globalmente no texto. Por exemplo, para substituir todas as ocorrências da palavra "Javascript" por "PHP", utilizamos o código a seguir:

```Javascript
var textoOriginal = "Eu amo programar em Javascript! Javascript é minha linguagem favorita!";
var novoTexto = textoOriginal.replace(/Javascript/g, "PHP");
console.log(novoTexto);

// Output: Eu amo programar em PHP! PHP é minha linguagem favorita!
```

No código acima, a flag `g` é adicionada à regex `/Javascript/g`, permitindo que todas as ocorrências da palavra sejam substituídas.

**Exemplo 3**: Adicionando variáveis à substituição

Podemos também utilizar variáveis na substituição de texto. Por exemplo, se tivermos uma lista de palavras que queremos substituir em um texto, podemos criar uma função que recebe essas palavras e realiza a substituição de forma automática. Veja o exemplo a seguir:

```Javascript
var textoOriginal = "Aqui temos uma lista de compras: frutas e legumes.";
var lista = ["frutas", "legumes"];
var novoTexto = textoOriginal.replace(/frutas|legumes/g, escolherItem);

function escolherItem(match) {
  if (match === "frutas") {
    return nomeFruta;
  } else if (match === "legumes") {
    return nomeLegume;
  }
}

console.log(novoTexto);

// Output: Aqui temos uma lista de compras: maçãs e cenouras.
```

No código acima, utilizamos a regex `/frutas|legumes/g` para substituir as palavras "frutas" e "legumes" por variáveis `nomeFruta` e `nomeLegume`, respectivamente.

## Aprofundando na busca e substituição de texto em Javascript

Existem inúmeras possibilidades de busca e substituição de texto em Javascript, utilizando diferentes expressões regulares e métodos. Recomendamos que você estude mais sobre o assunto para se tornar um expert no tema.

Além disso, é importante lembrar que as regex possuem diferentes recursos e sintaxes em cada linguagem de programação, então é sempre bom verificar a documentação do Javascript para ficar por dentro das melhores práticas e recursos atualizados.

# Veja também

- [Documentação oficial do método replace() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Guia de Regex em Javascript](https://www.regular-expressions.info/javascript.html)
- [Curso gratuito de Regex em Javascript](https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/regular-expressions/)

Obrigado por ler este artigo e esperamos ter ajudado a tornar seus projetos de programação mais ef