---
title:    "PHP: 디버그 출력하기."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜 디버그 출력을 사용해야 할까요?

프로그래밍을 하다 보면 우리는 코드의 실행 중에 어떤 문제가 발생했는지를 파악해야 할 때가 있습니다. 이때 디버그 출력은 매우 유용한 도구가 될 수 있습니다. 디버그 출력을 통해 실행 과정에서 변수의 값을 확인하거나 특정 코드 블록이 실행된 것을 확인할 수 있습니다. 디버그 출력 없이는 이러한 정보들을 얻기가 어려울 수 있습니다.

# 디버그 출력하는 법

```PHP
// 변수의 값을 확인하는 예시
$name = "John";
print "이름: " . $name; // 출력 결과: 이름: John

// 특정 코드 블록이 실행되었는지 확인하는 예시
function addNumbers($a, $b) {
    print "함수가 실행되었습니다.";
    return $a + $b;
}
$c = addNumbers(2, 3); // 출력 결과: 함수가 실행되었습니다.
```

# 디버그 출력에 대해 더 알아보기

디버그 출력은 실제로 많은 내부적인 작업을 수행합니다. PHP의 내장 함수인 `print`, `echo`를 이용하여 변수의 값을 출력할 수 있습니다. 또는 `var_dump`와 `print_r` 함수를 이용해 변수의 타입과 값의 상세한 정보를 출력할 수 있습니다.

# 관련 링크

- [PHP 디버그 출력 함수](https://www.php.net/manual/kr/function.print.php)
- [PHP: 디버깅 - Manual](https://www.php.net/manual/kr/book.debugger.php)
- [PHP 첨단 디버깅 기법](https://tech.kakao.com/2017/11/14/effective-debugging-in-php/)