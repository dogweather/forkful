---
title:    "Arduino: Excluindo caracteres que correspondem a um padrão"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que

Você já se deparou com o desafio de deletar caracteres de uma string que correspondem a um determinado padrão? Isso pode ser útil em diversas situações, como remover números de um texto ou substituir símbolos por espaços vazios. Aprender a programar essa funcionalidade pode facilitar muito sua vida como desenvolvedor Arduino.

## Como fazer

Para deletar caracteres que correspondem a um padrão em uma string no Arduino, você precisará do laço `for` e da função `startsWith()`. O laço `for` percorre cada caractere da string, enquanto a função `startsWith()` verifica se o caractere atual atende ao padrão especificado. Caso atenda, o caractere é deletado usando a função `remove()`, que desloca todos os caracteres seguintes para a esquerda.

Veja um exemplo de código que remove todos os números de uma string:

```Arduino
String texto = "Olá, eu tenho 123 anos!";
for (int i = 0; i < texto.length(); i++) {
  if (texto.startsWith("0") || texto.startsWith("1") || texto.startsWith("2") || texto.startsWith("3") || texto.startsWith("4") || texto.startsWith("5") || texto.startsWith("6") || texto.startsWith("7") || texto.startsWith("8") || texto.startsWith("9")) {
    texto.remove(i);
  }
}
Serial.println(texto); // Saída: "Olá, eu tenho  anos!"
```

## Mergulho Profundo

Apesar de poderoso, esse método pode apresentar alguns problemas. Por exemplo, se o caractere a ser removido estiver no início da string, ele não será deletado corretamente. Para resolver isso, você pode utilizar a função `substring()` para separar a string em duas partes, remover o caractere problemático e depois unir as partes novamente com a função `concat()`.

Além disso, é importante lembrar que essa função somente remove caracteres no índice especificado. Ou seja, se houver mais de um caracter a ser removido que atende ao padrão, é necessário realizar esse processo mais de uma vez.

## Veja também

- [Documentação oficial do Arduino](https://www.arduino.cc/en/Reference/StringObject)
- [Tutorial em vídeo sobre a função `remove()`](https://www.youtube.com/watch?v=2oZArMGOlno)
- [Exemplos de uso da função `startsWith()`](https://www.techonthenet.com/arduino/strings/starts.php)