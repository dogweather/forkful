---
title:                "테스트 작성하기"
html_title:           "PHP: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-tests.md"
---

{{< edit_this_page >}}

# 왜 테스트 코드를 작성해야 할까요?

코드를 작성할 때마다, 제대로 동작하는지를 확인하는 것은 매우 중요합니다. 이를 위해 테스트 코드를 작성하는 것은 코드의 신뢰성과 안정성을 높이기 위한 필수적인 작업입니다. 테스트 코드는 버그를 미리 발견하고 수정할 수 있도록 하며, 코드 변경사항이 예기치 않은 부작용을 일으키지 않도록 하는 역할을 합니다. 결론적으로, 테스트 코드는 안정적이고 신뢰성 높은 소프트웨어를 개발하는 데 도움이 됩니다.

# 방법: 테스트 코드 작성하기

첫 번째 단계는 간단한 예제 코드를 작성하는 것입니다. 예를 들어, 주어진 양수가 소수인지 판별하는 함수가 있다고 가정해봅시다.

```PHP
function isPrime($number) {
    if($number === 1) return false;
    if($number === 2) return true;

    for($i = 2; $i < $number; $i++) {
        if($number % $i === 0) return false;
    }

    return true;
}
```

위의 예제 코드에서는 `$number` 변수를 전달받아 1과 자기 자신 이외의 숫자로 나눠지지 않는지를 판별하여 `true` 또는 `false`를 반환하는 `isPrime` 함수를 만들었습니다.

이제 `isPrime` 함수가 정확하게 동작하는지를 확인하기 위해 테스트 코드를 작성해보겠습니다.

```PHP
// given
$number1 = 5;
$number2 = 9;
$number3 = 11;

// when
$result1 = isPrime($number1);
$result2 = isPrime($number2);
$result3 = isPrime($number3);

// then
echo "Is $number1 a prime number? " . ($result1 ? "Yes" : "No") . "\n";
echo "Is $number2 a prime number? " . ($result2 ? "Yes" : "No") . "\n";
echo "Is $number3 a prime number? " . ($result3 ? "Yes" : "No") . "\n";
```

위의 예제 코드에서는 `$number1`에는 소수인 5, `$number2`에는 소수가 아닌 9, `$number3`에는 소수인 11을 전달했습니다. 그리고 `isPrime` 함수를 호출한 결과를 `$result1`, `$result2`, `$result3` 변수에 할당한 뒤, 해당 숫자가 소수인지를 출력합니다.

실행 결과는 다음과 같이 나타납니다.

```
Is 5 a prime number? Yes
Is 9 a prime number? No
Is 11 a prime number? Yes
```

# 깊이 들어가보기

테스트 코드를 작성할 때 주의해야 할 점과 더 나은 테스트 코드를 작성하는 방법에 대해 알아보겠습니다.

첫 번째로, 테스트 코드를 작성할 때에는 모든 가능한 입력을 고려하는 것이 중요합니다. 위의 예제에서는 1과 2를 제외한 모든 양수를 검사하도록 하였지만, 해당 함수가 정수 이외의 다른 타입을 입력받았을 때 예기치 않은 동작을 할 수 있습니다. 따라서, 모든 가능한 유형의 입력에 대해 테스트하는 것이 좋습니다.

두 번째로, `assertTrue()`와 `assertFalse()`와 같은 테스트 어설션(assertion) 함수를 사용하여 코드의 동작을 확인하는 것이 좋습니다. 이를 통해 더 읽기 쉽고 명확한 테스트