---
title:                "PHP: 표준 에러 작성하기"
simple_title:         "표준 에러 작성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
왜 누군가 표준 오류를 작성하는 작업에 관여 할까요? 그 이유는 디버깅이나 로그 기록에 매우 유용하기 때문입니다.

## 어떻게
표준 오류를 작성하는 방법은 간단합니다. 먼저 `error_log()` 함수를 사용하여 원하는 메시지를 작성하고, 두 번째 매개 변수에 적절한 로그 파일 경로를 지정합니다. 아래는 PHP에서 표준 오류를 작성하는 예제 코드입니다.

```PHP
<?php
// 에러 메시지 작성
$error_msg = "Oops! Something went wrong.";

// 로그 파일 경로 설정
$log_file = "/var/log/php_errors.log";

// 표준 오류 작성
error_log($error_msg, 3, $log_file);
?>
```

위 코드를 실행하면 `/var/log/php_errors.log` 파일에 `Oops! Something went wrong.`이라는 메시지가 기록됩니다.

## 딥 다이브
표준 오류에 대한 자세한 내용은 PHP에서 에러 처리를 다루는 방법을 알아야합니다. 예를 들어, `error_reporting` 및 `display_errors`와 같은 설정을 사용하여 어떤 유형의 에러를 표시할지 결정할 수 있습니다. 또한 `trigger_error()` 함수를 사용하여 코드 내에서 직접 오류를 발생시킬 수도 있습니다. 이러한 기능을 이용하면 더욱 정교한 에러 처리를 구현할 수 있습니다.

## 관련 자료
다른 PHP 사용자들에게 도움이 될 수 있는 자료들입니다.

- [PHP 공식 문서 - error_log()](https://www.php.net/manual/en/function.error-log.php)
- [PHP 공식 문서 - error_reporting](https://www.php.net/manual/en/function.error-reporting.php)
- [PHP 공식 문서 - trigger_error()](https://www.php.net/manual/en/function.trigger-error.php)