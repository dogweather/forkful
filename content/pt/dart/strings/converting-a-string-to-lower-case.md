---
title:                "Convertendo uma string para letras minúsculas"
date:                  2024-03-08T21:53:59.740307-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Converter uma string para letras minúsculas é uma operação fundamental que envolve transformar todos os caracteres de uma string dada em seus equivalentes em minúscula. Os programadores geralmente realizam essa operação para alcançar comparações que não distinguem maiúsculas de minúsculas ou para padronizar a entrada de texto para processamento adicional, tornando os aplicativos mais amigáveis ao usuário e os dados mais consistentes.

## Como fazer:

No Dart, você pode converter uma string para minúscula usando o método `toLowerCase()` fornecido pela classe `String`. Este método retorna uma nova string com todos os caracteres em maiúscula convertidos para minúscula. Vamos ver como isso funciona com um exemplo simples:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Saída: hello, world!
}
```

O Dart não requer bibliotecas externas para tarefas básicas de manipulação de string, incluindo a conversão para minúscula, visto que a classe `String` da biblioteca padrão é bastante abrangente. No entanto, para manipulações mais complexas envolvendo regras específicas de localidade, você pode considerar o pacote `intl`, que fornece facilidades de internacionalização e localização, incluindo a conversão de caixa baseada em localidade:

Para usar o `intl`, adicione-o ao seu arquivo `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Então, você pode usar o método `toLocaleLowerCase()` para converter uma string para minúscula com base em localidades específicas:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Localidade Turca
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Saída: istanbul
  
  // Localidade Padrão (en)
  print(originalString.toLowerCase()); // Saída: i̇stanbul
}
```

Neste exemplo, observe como a localidade turca lida corretamente com o 'i' sem ponto, destacando a importância das transformações conscientes de localidade em aplicações internacionalizadas.
