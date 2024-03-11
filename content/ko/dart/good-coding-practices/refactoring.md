---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:20.858504-07:00
description: "Dart\uC5D0\uC11C \uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCF54\uB4DC\
  \uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC73C\uBA74\uC11C \uB0B4\uBD80 \uAD6C\
  \uC870, \uAC00\uB3C5\uC131 \uBC0F \uC720\uC9C0 \uAD00\uB9AC\uB97C \uAC1C\uC120\uD558\
  \uAE30 \uC704\uD55C \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354 \uAE54\uB054\uD558\uAC8C, \uC774\uD574\uD558\
  \uAE30 \uC27D\uAC8C \uB610\uB294 \uB354 \uD6A8\uC728\uC801\uC73C\uB85C \uB9CC\uB4E4\
  \uAE30 \uC704\uD574 \uC885\uC885 \uB9AC\uD329\uD1A0\uB9C1\uC744 \uD569\uB2C8\uB2E4\
  . \uC774\uB294 \uD5A5\uD6C4 \uC218\uC815\uC774\u2026"
lastmod: '2024-03-11T00:14:28.720355-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCF54\uB4DC\
  \uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC73C\uBA74\uC11C \uB0B4\uBD80 \uAD6C\
  \uC870, \uAC00\uB3C5\uC131 \uBC0F \uC720\uC9C0 \uAD00\uB9AC\uB97C \uAC1C\uC120\uD558\
  \uAE30 \uC704\uD55C \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354 \uAE54\uB054\uD558\uAC8C, \uC774\uD574\uD558\
  \uAE30 \uC27D\uAC8C \uB610\uB294 \uB354 \uD6A8\uC728\uC801\uC73C\uB85C \uB9CC\uB4E4\
  \uAE30 \uC704\uD574 \uC885\uC885 \uB9AC\uD329\uD1A0\uB9C1\uC744 \uD569\uB2C8\uB2E4\
  . \uC774\uB294 \uD5A5\uD6C4 \uC218\uC815\uC774\u2026"
title: "\uB9AC\uD329\uD1A0\uB9C1"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart에서 리팩토링은 기존 코드의 구조를 변경하지 않고 외부 동작을 변경하지 않으면서 내부 구조, 가독성 및 유지 관리를 개선하기 위한 과정입니다. 프로그래머들은 코드를 더 깔끔하게, 이해하기 쉽게 또는 더 효율적으로 만들기 위해 종종 리팩토링을 합니다. 이는 향후 수정이 더 용이해지고 버그 발생 가능성이 감소하도록 돕습니다.

## 방법:

### 예시 1: 이름 변경 및 메소드 추출하기

리팩토링 전에는 할인을 계산하고 적용하는 등 다양한 수준의 추상화나 책임을 혼합한 코드가 있을 수 있습니다:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("최종 가격: $finalPrice");
}
```

**출력:**
```
최종 가격: 80.0
```

리팩토링 후에는 할인 계산을 자체 메소드로 추출하고 의미 있는 이름을 지정할 수 있습니다:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("최종 가격: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**출력:**
```
최종 가격: 80.0
```

계산을 메소드로 추출함으로써 재사용이 가능하고 독립적으로 테스트하며 쉽게 수정할 수 있는 명확하게 정의된 작업을 갖게 되었습니다.

### 예시 2: 조건식 간소화하기

리팩토링 전에는 조건문이 너무 복잡하거나 읽기 어려울 수 있습니다:

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

  print("할인: $discount");
}
```

**출력:**
```
할인: 0.05
```

리팩토링 후 고객 유형과 할인에 대한 업데이트나 확장이 더 쉬운 구조를 위해 맵을 사용하는 것을 고려해 보세요:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("할인: $discount");
}
```

**출력:**
```
할인: 0.05
```

이 리팩토링은 코드를 더 간결하게 만들 뿐만 아니라 할인을 결정하는 로직을 이해하고 유지하기 더 쉬운 방식으로 캡슐화합니다.

### 제3자 라이브러리를 이용한 리팩토링

Dart에서 특히 Flutter 앱 내에서 리팩토링을 할 때는 [Dart DevTools](https://dart.dev/tools/dart-devtools) 스위트가 매우 중요합니다. 성능 도구, 위젯 인스펙터, 소스 레벨 디버거가 포함되어 있습니다. Dart DevTools는 제3자 라이브러리는 아니지만, 리팩토링을 통해 모듈성과 가독성을 개선하기 위해 `flutter_bloc` 같은 라이브러리와 함께 자주 사용됩니다. 이 항목의 범위 때문에 제3자 라이브러리를 사용한 구체적인 코드 예시는 여기서 제공되지 않지만, 개발자들은 이러한 도구를 탐색하여 자신의 Dart/Flutter 애플리케이션에서 리팩토링 과정을 향상시키도록 권장됩니다.
