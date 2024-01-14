---
title:                "PHP: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/printing-debug-output.md"
---

{{< edit_this_page >}}

"## 왜 디버그 출력을 활용해야 할까?"

디버그 출력을 통해 우리는 소스 코드의 실행 과정을 쉽게 추적할 수 있으며, 오류를 더 쉽게 파악할 수 있습니다.

"## 디버그 출력하는 방법"

```PHP
// 디버그 출력을 활성화합니다.
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

// 디버그 출력을 통해 변수의 값과 로그 메시지를 출력합니다.
$variable = "Hello World";
error_log("변수의 값: " . $variable);
```

출력 예시:

```
변수의 값: Hello World
```

"## 딥 다이브"

디버그 출력을 활용하면 우리는 프로그램 실행 중에 변수의 값이 어떻게 변하는지를 쉽게 확인할 수 있습니다. 또한, 로그 메시지를 출력하여 코드의 실행 상태를 더 자세히 파악할 수도 있습니다.

"# 참고 자료"

- [PHP 디버그 출력 활성화하기](https://www.php.net/manual/en/function.error-reporting.php)
- [PHP 변수 출력하기](https://www.php.net/manual/en/function.error-log.php)
- [PHP 에러 메시지 출력하기](https://www.php.net/manual/en/function.error-log.php)