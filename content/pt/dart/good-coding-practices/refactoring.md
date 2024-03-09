---
title:                "Refatoração"
date:                  2024-03-08T21:56:35.672200-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Refatoração em Dart é o processo de reestruturar o código existente sem alterar seu comportamento externo, visando melhorar sua estrutura interna, legibilidade e manutenibilidade. Programadores frequentemente refatoram para tornar o código mais limpo, fácil de entender ou mais eficiente, facilitando modificações futuras e diminuindo a probabilidade de bugs.

## Como Fazer:

### Exemplo 1: Renomeando e Extraindo Métodos

Antes da refatoração, você pode ter um trecho de código que mistura diferentes níveis de abstração ou responsabilidades, como calcular um desconto e depois aplicá-lo:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Preço final: $finalPrice");
}
```

**Saída:**
```
Preço final: 80.0
```

Após a refatoração, você pode extrair o cálculo do desconto para seu próprio método e dar a ele um nome significativo:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calcularPrecoFinal(price, discount);
  print("Preço final: $finalPrice");
}

double calcularPrecoFinal(double price, double discount) {
  return price - (price * discount);
}
```

**Saída:**
```
Preço final: 80.0
```

Ao extrair o cálculo para um método, você agora tem uma operação claramente definida que pode ser reutilizada, testada independentemente e facilmente modificada.

### Exemplo 2: Simplificando Expressões Condicionais

Antes da refatoração, as declarações condicionais podem ser excessivamente complexas ou difíceis de ler:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Desconto: $discount");
}
```

**Saída:**
```
Desconto: 0.05
```

Após a refatoração, considere usar um mapa para uma estrutura mais clara e atualizações ou extensões mais fáceis para tipos de clientes e descontos:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "nenhum": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Desconto: $discount");
}
```

**Saída:**
```
Desconto: 0.05
```

Esta refatoração não apenas torna o código mais conciso, mas também encapsula a lógica para determinar descontos de uma maneira mais fácil de entender e manter.

### Bibliotecas de Terceiros para Refatoração

Quando se trata de refatoração em Dart, especialmente dentro de aplicativos Flutter, o conjunto de ferramentas [Dart DevTools](https://dart.dev/tools/dart-devtools) é inestimável. Inclui ferramentas de desempenho, um inspetor de widget e um depurador em nível de código-fonte. Embora não seja uma biblioteca de terceiros, Dart DevTools é frequentemente usado junto com bibliotecas como `flutter_bloc` para gerenciar o estado de maneira limpa de forma que favoreça a refatoração para melhor modularidade e legibilidade. Infelizmente, devido ao escopo desta entrada, exemplos de código específicos usando bibliotecas de terceiros não serão fornecidos aqui, mas os desenvolvedores são encorajados a explorar essas ferramentas para aprimorar o processo de refatoração em suas aplicações Dart/Flutter.
