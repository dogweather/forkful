---
title:                "Trabalhando com json"
html_title:           "TypeScript: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## O que e por que?

Trabalhar com JSON é uma parte fundamental da programação em TypeScript. JSON, também conhecido como JavaScript Object Notation, é um formato de dados leve e fácil de ler que é amplamente utilizado para armazenar e transmitir informações na web. Programadores usam o JSON para criar e manipular objetos de dados, tornando-o uma ferramenta valiosa para o desenvolvimento de aplicativos baseados na web.

## Como fazer:

Aqui está um exemplo simples de como trabalhar com JSON no TypeScript:

```TypeScript
// Criando um objeto de dados em JSON
let car = {
  "brand": "Toyota",
  "model": "Corolla",
  "year": 2020
};

// Convertendo o objeto em uma string JSON usando a função stringify
let jsonString = JSON.stringify(car);
console.log(jsonString);
// Resultado: {"brand": "Toyota", "model": "Corolla", "year": 2020}

// Convertendo uma string JSON de volta para um objeto usando a função parse
let carObj = JSON.parse(jsonString);
console.log(carObj);
// Resultado: { brand: "Toyota", model: "Corolla", year: 2020 }
```

## Aprofundando-se:

O JSON foi criado no início dos anos 2000 e se tornou um formato popular para a troca de dados em aplicativos web. Seus principais concorrentes são o XML e o YAML, mas o JSON é amplamente preferido por sua simplicidade e leveza. Em relação ao TypeScript, o JSON é uma opção mais amigável para ser usado como linguagem de marcação em comparação com o XML, por exemplo. Além disso, o TypeScript possui interfaces e tipos de dados que podem ser usados para validar e garantir que os objetos JSON estejam estruturados corretamente.

## Veja também:

Para saber mais sobre como trabalhar com JSON em TypeScript, confira a documentação oficial da linguagem e o guia de referência da biblioteca nativa JSON no site do TypeScript. Além disso, você pode explorar outras opções de bibliotecas externas disponíveis para trabalhar com JSON no TypeScript, como o popular pacote "json2typescript". Com essas ferramentas, você pode facilitar ainda mais sua experiência de desenvolvimento com JSON em projetos TypeScript.