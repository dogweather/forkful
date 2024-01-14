---
title:    "Arduino: Buscando e substituindo texto"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que
Muitas vezes, quando estamos programando em Arduino, nos deparamos com a necessidade de fazer alterações em um texto, seja modificando uma variável ou procurando parâmetros específicos. Nesses casos, é extremamente útil saber como fazer buscas e substituições de texto dentro do código.

## Como fazer
Para fazer buscas e substituições de texto em seu código Arduino, você pode seguir os seguintes passos:

1. Abra o seu código no software de programação do Arduino.
2. Clique em "Editar" no menu superior e selecione "Substituir" (ou pressione "Ctrl + H" no teclado).
3. Na caixa de diálogo que abrir, digite o texto que deseja substituir no campo "Localizar" e o texto que deseja utilizar no campo "Substituir por".
4. Clique em "Substituir" ou "Substituir tudo" para fazer as alterações desejadas no seu código.

Aqui está um exemplo de como fazer uma substituição simples no código em Arduino:

```Arduino
void setup() {
  Serial.begin(9600);
  String mensagem = "Olá Mundo!";
  mensagem.replace("Olá", "Oi");
  Serial.println(mensagem);
}
```

Output:
```
Oi Mundo!
```

## Aprofundando
Se você precisa fazer buscas e substituições mais complexas, é possível utilizar expressões regulares no software do Arduino. As expressões regulares são padrões utilizados para encontrar e manipular textos de forma mais avançada.

Por exemplo, se você quiser substituir todos os números em uma string por asteriscos, pode usar o seguinte código:

```Arduino
void setup() {
  Serial.begin(9600);
  String texto = "Eu tenho 5 gatos e 3 cachorros.";
  texto.replaceAll("[0-9]", "*");
  Serial.println(texto);
}
```

Output:
```
Eu tenho * gatos e * cachorros.
```

Note que, para utilizar expressões regulares, é preciso incluir a biblioteca "regex.h" no seu código. Além disso, é importante estudar e entender bem a sintaxe das expressões regulares antes de utilizá-las.

## Veja também
- [Tutorial sobre expressões regulares em Arduino](https://www.arduino.cc/reference/en/language/functions/string-functions/regex_/)
- [Exemplo de substituição com expressões regulares em Arduino](https://forum.arduino.cc/t/how-to-use-regex-functions-of-arduino/487680/3)
- [Vídeo explicando como fazer substituições em texto no Arduino](https://www.youtube.com/watch?v=Z5rRZdiu1UE)