---
title:                "Trabalhando com JSON"
html_title:           "Javascript: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é uma forma popular e eficiente de armazenar e transmitir dados estruturados na web. Ao usar JSON, os desenvolvedores podem facilmente trocar informações entre diferentes aplicativos e linguagens de programação. Ao aprender como trabalhar com JSON, você pode tornar seus projetos mais dinâmicos e interativos.

## Como fazer

Para começar a trabalhar com JSON, siga os passos abaixo:

1. Crie um objeto JavaScript com dados estruturados, como por exemplo:
```Javascript
let pessoa = {
  "nome": "João",
  "idade": 25,
  "profissão": "Desenvolvedor"
};
```
2. Use a função `JSON.stringify()` para converter o objeto em uma string JSON:
```Javascript
let jsonPessoa = JSON.stringify(pessoa);
```
3. Agora você pode transmitir a informação JSON para outro aplicativo ou armazená-la em um arquivo. Para recuperar os dados, use a função `JSON.parse()`, que converterá a string JSON de volta para um objeto JavaScript.
```Javascript
let pessoaDevolvida = JSON.parse(jsonPessoa);

console.log(pessoaDevolvida.nome); // Saída: João
console.log(pessoaDevolvida.idade); // Saída: 25
console.log(pessoaDevolvida.profissão); // Saída: Desenvolvedor
```

## Mergulho profundo

Além de converter objetos JavaScript em strings JSON e vice-versa, há muitas outras maneiras de trabalhar com JSON. Aqui estão algumas dicas para se aprofundar ainda mais no assunto:

- Use a função `JSON.stringify()` com um terceiro parâmetro (conhecido como "espaceamento") para formatar a string JSON de forma mais legível.
- Use a propriedade `type` em objetos JSON para indicar o tipo de dados, como por exemplo: `"type": "number"` ou `"type": "string"`.
- Adicione comentários de documentação em seu código JSON usando a sintaxe `//` ou `/* */`.

## Veja também

Para saber mais sobre como trabalhar com JSON em JavaScript, confira esses recursos úteis:

- [Documentação oficial do JSON](https://www.json.org/json-en.html)
- [Exemplos práticos de uso de JSON com JavaScript](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
- [Manipulando objetos JSON com JavaScript](https://www.w3schools.com/js/js_json.asp)