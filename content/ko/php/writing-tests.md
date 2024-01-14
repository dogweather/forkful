---
title:    "PHP: 테스트 작성하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

테스트를 작성하는 것의 중요성은 개발자가 코드를 작성할 때 실수를 줄일 수 있도록 도와주고 안정성과 신뢰성을 높일 수 있기 때문입니다.

## 하는 방법

이제 우리는 PHP를 사용하여 간단한 테스트를 작성하는 방법을 살펴보겠습니다. 우리는 곱셈 함수를 작성하고 해당 함수가 제대로 작동하는지 테스트해볼 것입니다.

```PHP
<?php
function multiply($x, $y) {
  return $x * $y;
}

echo multiply(3, 5); // Output: 15
```

위의 예제 코드를 보면, 제대로 작동하지 않는 경우에 대한 테스트가 없습니다. 따라서 테스트를 작성하여 해당 함수가 제대로 작동하는지 확인할 수 있도록 해보겠습니다.

```PHP
<?php
function multiply($x, $y) {
  return $x * $y;
}

// Test 1 - Multiplying positive numbers
$result = multiply(3, 5);
if ($result === 15) {
  echo "Test 1 passed!";
} else {
  echo "Test 1 failed!";
}

// Test 2 - Multiplying negative and positive numbers
$result = multiply(-3, 5);
if ($result === -15) {
  echo "Test 2 passed!";
} else {
  echo "Test 2 failed!";
}
```

위의 테스트 코드에서는 두 가지 경우에 대한 테스트를 작성했습니다. 이 코드를 실행하면 두 테스트 모두 통과하게 될 것입니다.

## 깊이 들어가기

테스트 코드를 작성할 때는 더 많은 시나리오에 대한 테스트를 추가하는 것이 좋습니다. 예를 들어, 위의 예제에서는 곱셈을 한 결과값만을 테스트하였지만, 곱셈 함수에서 0을 포함한 다른 인수에 대한 테스트도 추가해보는 것이 좋습니다. 또한, 위의 테스트 코드에서는 echo를 사용하여 테스트 결과를 출력하였지만, 실제로는 테스트를 위한 전용 라이브러리를 사용하는 것이 더 바람직합니다. 이러한 방법을 통해 코드를 더 효율적으로 테스트할 수 있으며, 필요에 따라 추가적인 기능도 제공할 수 있습니다.

## 또 다른 자료

- [PHPUnit 홈페이지](https://phpunit.de/)
- [TDD란 무엇인가?](https://www.agilealliance.org/glossary/tdd/)
- [테스트 주도 개발: 단위 테스트로 시작하기](https://medium.com/@haho66200/tdd-%ED%85%8C%EC%8A%A4%ED%8A%B8-%EC%A3%BC%EB%8F%84-%EA%B8%B0%EB%8A%A5-%EC%8B%9C%EC%9E%91%ED%95%98%EA%B8%B0-c72c9e4b3921)

## 더 보기

- [PHP에서 테스트하기: PHPUnit을 사용하여 단위 테스트 작성](https://www.toptal.com/php/your-php-unit-testing-explained)
- [PHP 단위 테스트를 지탱하는 기본 개념](https://thisinterestsme.com/unit-testing-php/)
- [PHP 테스트 코드 작성하기](https://www.w3schools.in/php/learn/php-test-code/)