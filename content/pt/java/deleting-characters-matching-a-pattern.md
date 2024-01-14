---
title:    "Java: Excluindo caracteres que correspondem a um padrão"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Sabe quando você tem um texto grande e precisa remover certos caracteres que seguem um padrão específico? Fazer isso manualmente pode ser um processo demorado e até mesmo tedioso. É aí que entra a função de deleteCharAt() do Java. Ao entender como usá-la corretamente, você pode economizar tempo e esforço ao lidar com strings em suas aplicações.

## Como fazer

Para começar, vamos supor que temos uma string chamada "Java Programming" e queremos remover todas as letras "a" dessa string. Usando a função deleteCharAt(), podemos fazer isso facilmente.

```Java
String texto = "Java Programming";
StringBuilder sb = new StringBuilder(texto);

for (int i = 0; i < sb.length(); i++) {
  if (sb.charAt(i) == 'a') {
    sb.deleteCharAt(i);
  }
}

System.out.println(sb.toString());
```

O código acima irá iterar através da string e, toda vez que encontrar a letra "a", irá remover esse caractere usando a função deleteCharAt(). No final, o output será "Jv Progrmming", onde todas as letras "a" foram removidas.

Agora, vamos dar um exemplo de como podemos usar a função para remover números de uma string. Suponha que temos a seguinte string: "123Hello456World78". Nós queremos remover todos os dígitos dessa string para ficar apenas com as palavras "Hello World". Podemos fazer isso da seguinte maneira:

```Java
String texto = "123Hello456World78";
StringBuilder sb = new StringBuilder(texto);

for (int i = 0; i < sb.length(); i++) {
  if (Character.isDigit(sb.charAt(i))) {
    sb.deleteCharAt(i);
    i--; //precisamos diminuir o contador para manter a posição correta
  }
}

System.out.println(sb.toString());
```

Neste exemplo, usamos a função isDigit() para verificar se o caractere atual é um dígito. Se for, removemos o caractere e diminuímos o contador para manter a posição correta ao iterar pela string. O output final será "Hello World", sem os números.

## Deep Dive

A função deleteCharAt() pertence à classe StringBuilder do Java e é usada para remover um caractere específico em uma posição específica em uma string. A string original não é modificada, já que a função retorna uma nova string com o caractere removido.

Além disso, quando usamos deleteCharAt(), precisamos ter cuidado com o deslocamento dos caracteres. Ao remover um caractere em uma determinada posição, os caracteres subsequentes são deslocados para preencher o espaço deixado pelo caractere removido. Portanto, pode ser necessário diminuir o contador ao iterar pela string, como no exemplo anterior.

## Veja também

- Documentação oficial do Java para a função deleteCharAt(): https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html#deleteCharAt-int-
- Um tutorial sobre manipulação de strings em Java: https://www.baeldung.com/java-delete-specific-characters-from-string
- Exemplos adicionais de uso da função deleteCharAt(): https://www.tutorialspoint.com/java/lang/stringbuilder_deletecharat.htm