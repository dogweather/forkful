---
date: 2024-01-26 00:56:10.702204-07:00
description: "PHP\uC5D0\uC11C\uC758 \uC624\uB958 \uCC98\uB9AC\uB294 \uD504\uB85C\uADF8\
  \uB7A8\uC758 \uC815\uC0C1\uC801\uC778 \uD750\uB984\uC744 \uBC29\uD574\uD558\uB294\
  \ \uC870\uAC74, \uC608\uB97C \uB4E4\uC5B4 \uD30C\uC77C\uC774 \uC5C6\uAC70\uB098\
  \ \uC798\uBABB\uB41C \uB370\uC774\uD130 \uC785\uB825\uACFC \uAC19\uC740 \uC0C1\uD669\
  \uC744 \uAD00\uB9AC\uD558\uACE0 \uB300\uC751\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC624\uB958\uB97C \uCC98\
  \uB9AC\uD558\uC5EC \uCDA9\uB3CC\uC744 \uBC29\uC9C0\uD558\uACE0 \uC0AC\uC6A9\uC790\
  \uC5D0\uAC8C \uB354 \uC6D0\uD65C\uD55C \uACBD\uD5D8\uC744 \uC81C\uACF5\uD558\uAE30\
  \ \uC704\uD574 \uB178\uB825\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.367656-06:00'
model: gpt-4-1106-preview
summary: "PHP\uC5D0\uC11C\uC758 \uC624\uB958 \uCC98\uB9AC\uB294 \uD504\uB85C\uADF8\
  \uB7A8\uC758 \uC815\uC0C1\uC801\uC778 \uD750\uB984\uC744 \uBC29\uD574\uD558\uB294\
  \ \uC870\uAC74, \uC608\uB97C \uB4E4\uC5B4 \uD30C\uC77C\uC774 \uC5C6\uAC70\uB098\
  \ \uC798\uBABB\uB41C \uB370\uC774\uD130 \uC785\uB825\uACFC \uAC19\uC740 \uC0C1\uD669\
  \uC744 \uAD00\uB9AC\uD558\uACE0 \uB300\uC751\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC624\uB958\uB97C \uCC98\
  \uB9AC\uD558\uC5EC \uCDA9\uB3CC\uC744 \uBC29\uC9C0\uD558\uACE0 \uC0AC\uC6A9\uC790\
  \uC5D0\uAC8C \uB354 \uC6D0\uD65C\uD55C \uACBD\uD5D8\uC744 \uC81C\uACF5\uD558\uAE30\
  \ \uC704\uD574 \uB178\uB825\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
PHP에서의 오류 처리는 프로그램의 정상적인 흐름을 방해하는 조건, 예를 들어 파일이 없거나 잘못된 데이터 입력과 같은 상황을 관리하고 대응하는 것을 말합니다. 프로그래머들은 오류를 처리하여 충돌을 방지하고 사용자에게 더 원활한 경험을 제공하기 위해 노력합니다.

## 실행 방법:
PHP에서는 `try-catch` 블록을 사용하여 오류를 관리할 수 있고, 사용자 정의 오류 핸들러 및 예외를 사용하여 처리 과정을 사용자화 할 수 있습니다.

```php
// 기본 try-catch 예제
try {
  // 위험한 작업을 수행
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // 오류 처리
  echo "Error: " . $e->getMessage();
}

// 사용자 정의 오류 핸들러 설정
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// 예외 사용
class MyException extends Exception {}

try {
  // 무언가를 하고 사용자 정의 예외를 발생시킴
  throw new MyException("Custom error!");
} catch (MyException $e) {
  // 사용자 정의 예외 처리
  echo $e->getMessage();
}

// 샘플 출력:
// Error: fopen(nonexistentfile.txt): failed to open stream: No such file or directory
// Custom error!
```

## 심층 분석
예전에 PHP 오류는 스크립트 실행을 멈추지 않는 경고와 알림 같은 것들이었습니다. 언어가 성숙해지면서 PHP 5에서 도입된 Exception 클래스를 통한 더 견고한 객체 지향 오류 처리를 채택했습니다. 나중에 PHP 7이 도입되면서 오류와 예외를 구분하는 Error 클래스가 나왔습니다.

`try-catch` 블록이 도입되기 전에 PHP는 `set_error_handler()`를 사용하여 오류를 처리했습니다. `try-catch`는 더 깔끔하고 현대적입니다. 그러나 사용자 정의 오류 핸들러는 특히 레거시 코드를 다루거나 일반적으로 예외가 아닌 오류를 잡아야 할 때 여전히 자리를 차지하고 있습니다.

PHP 7+의 `Throwable` 인터페이스는 오류든 예외든 모두 잡을 수 있다는 것을 의미합니다. 이는 이전에 추적하기 어려웠던 심각한 런타임 오류를 놓치지 않게 해주므로 유용합니다.

PHP의 내장 메커니즘 외부의 대안으로는 파일에 오류를 기록하거나 사용자 친화적인 오류 페이지를 표시하는 것과 같은 더 많은 기능을 제공하는 라이브러리 및 프레임워크가 있습니다.

## 참고 자료
- 예외에 대한 공식 PHP 문서: https://www.php.net/manual/en/language.exceptions.php
- 오류 보고에 관한 PHP The Right Way: https://phptherightway.com/#error_reporting
- 오류 처리에 관한 PHP 매뉴얼: https://www.php.net/manual/en/book.errorfunc.php
