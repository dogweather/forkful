---
title:                "Trabalhando com json"
html_title:           "Java: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON? 

JSON (JavaScript Object Notation) é um formato popular para armazenar e trocar dados na web. Ele é leve, flexível e fácil de entender, tornando-o uma ótima escolha para comunicação entre diferentes sistemas. Ao trabalhar com JSON, é possível estruturar e manipular dados de forma eficiente, permitindo a criação de aplicações dinâmicas e modernas.

## Como fazer:

Para começar a trabalhar com JSON em Java, é necessário importar a biblioteca `org.json`. Em seguida, pode-se utilizar a classe `JSONObject` para criar um objeto JSON a partir de uma string ou um mapa. Veja um exemplo abaixo:

```Java
import org.json.*;

String jsonStr = "{\"nome\":\"João\",\"idade\":25}";
JSONObject jsonObj = new JSONObject(jsonStr);
System.out.println(jsonObj.getString("nome")); //Saída: João
System.out.println(jsonObj.getInt("idade")); //Saída: 25
```

Também é possível criar um objeto JSON a partir de um objeto Java utilizando a classe `JSONObject` ou `JSONArray`. Veja o exemplo abaixo:

```Java
import org.json.*;

class Pessoa {
  private String nome;
  private int idade;

  public Pessoa(String nome, int idade) {
    this.nome = nome;
    this.idade = idade;
  }

  public String getNome() {
    return nome;
  }

  public int getIdade() {
    return idade;
  }
}

JSONObject jsonObj = new JSONObject(new Pessoa("Maria", 30));
System.out.println(jsonObj.toString()); //Saída: {"nome":"Maria","idade":30}
```

Para acessar os valores de um objeto JSON, é possível utilizar os métodos `get()` ou `opt()`. Enquanto o primeiro retorna uma exceção caso a chave não exista, o segundo retorna `null` ou um valor padrão. Veja o exemplo abaixo:

```Java
JSONObject jsonObj = new JSONObject("{\"nome\":\"José\"}");
System.out.println(jsonObj.get("idade")); //Exceção: Chave não encontrada
System.out.println(jsonObj.optString("idade")); //Saída: null
System.out.println(jsonObj.optString("idade", "Valor padrão")); 
//Saída: Valor padrão
```

Para criar um array JSON, basta utilizar a classe `JSONArray` e adicionar os elementos desejados. Veja o exemplo abaixo:

```Java
JSONArray jsonArr = new JSONArray();
jsonArr.put("Maçã");
jsonArr.put("Banana");
jsonArr.put("Morango");
System.out.println(jsonArr.toString()); //Saída: ["Maçã","Banana","Morango"]
```

## Aprofundando-se:

Ao trabalhar com JSON em Java, é importante lembrar que ele deve ser tratado como um objeto e não como uma string. Isso significa que é necessário validar e manipular os dados corretamente, para evitar possíveis erros.

Também é possível converter um objeto JSON em uma string e vice-versa, utilizando os métodos `toString()` e `toJSONString()`. E caso seja necessário trabalhar com documentos JSON maiores, pode-se utilizar a biblioteca `Jackson`, que oferece uma melhor performance e recursos adicionais.

Além disso, é importante conhecer a estrutura de um documento JSON, que é composto por uma coleção de pares de chave-valor, separados por vírgula e delimitados por chaves `{}`. Existem também os arrays JSON, que são coleções de valores separados por vírgula e delimitados por colchetes `[]`.

## Veja também:

- [Documentação oficial do org.json](https://stleary.github.io/JSON-java/)
- [Documentação oficial do Jackson](https://github.com/FasterXML/jackson)
- [Tutorial de JSON em Java da Baeldung (em inglês)](https://www.baeldung.com/java-json)