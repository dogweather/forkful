---
title:                "Java: Trabalhando com o json"
simple_title:         "Trabalhando com o json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON no Java?

JSON (JavaScript Object Notation) é um formato popular para troca de dados estruturados pela internet. É leve e fácil para humanos e máquinas compreenderem, e por isso é amplamente utilizado em aplicações web e móveis. Trabalhar com JSON no Java permite que você facilmente leia, escreva e manipule dados em formato JSON em suas aplicações.

## Como fazer:

Para trabalhar com JSON no Java, é necessário primeiro adicionar a biblioteca do JSON à sua aplicação. Você pode usar uma ferramenta de gerenciamento de dependência como o Maven para isso, ou simplesmente baixar o arquivo JAR e adicioná-lo ao caminho de classpath da sua aplicação.

Uma vez que a biblioteca esteja adicionada, você pode começar a ler e escrever dados em formato JSON. Aqui está um exemplo simples de como criar um objeto JSON e escrevê-lo para um arquivo:

```Java
// Importe a biblioteca JSON
import org.json.simple.JSONObject;

public class ExemploJSON {
  public static void main(String[] args) {
    // Cria um objeto JSON
    JSONObject pessoa = new JSONObject();
    pessoa.put("nome", "Maria");
    pessoa.put("idade", 30);

    // Escreve o objeto JSON para um arquivo
    try (FileWriter escritor = new FileWriter("pessoa.json")) {
      escritor.write(pessoa.toJSONString());
    } catch (IOException e) {
      System.out.println("Erro ao escrever arquivo JSON: " + e.getMessage());
    }
  }
}
```

O código acima criará um arquivo chamado "pessoa.json" com o conteúdo `{"nome":"Maria","idade":30}`.

Para ler dados de um arquivo JSON, você pode usar uma classe como o `FileReader` para ler o conteúdo do arquivo e então usar o método `JSONObject.parse()` para transformar os dados em um objeto JSON que pode ser manipulado em seu código.

## Profundamente:

Trabalhar com JSON em Java pode ser estendido além de simplesmente ler e escrever dados. Existem muitas bibliotecas e ferramentas que podem ser utilizadas para validar, transformar e mapear dados JSON em objetos Java.

Uma dessas ferramentas é o Jackson, que permite mapear dados JSON diretamente para objetos Java e vice-versa. Isso é útil quando você tem uma API que retorna dados em JSON e precisa convertê-los em objetos Java para utilizá-los em sua aplicação.

Outra ferramenta útil é o Gson, que permite validar e transformar dados JSON em objetos Java usando anotações. Isso pode facilitar muito o processo de conversão e validação de dados.

## Veja também:

- [JSON no Java: Um guia para iniciantes](https://www.baeldung.com/java-json)
- [Documentação da biblioteca JSON Simple](https://code.google.com/archive/p/json-simple/)
- [Jackson - Guia de usuário](https://github.com/FasterXML/jackson-docs/wiki/Jackson-JSON-Serialization%3A-Starting-Off)