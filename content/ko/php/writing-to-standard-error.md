---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
표준 에러 쓰기(standard error writing)는 오류 메시지나 경고를 출력하는 방법입니다. 프로그래머들은 프로그램의 표준 출력과 에러 메시지를 분리하여 더 나은 로깅과 사용자 피드백을 제공하기 위해 이 방법을 사용합니다.

## How to: (어떻게 하나요?)
```php
<?php
// 표준 에러에 메시지 쓰기
fwrite(STDERR, "이건 에러 메시지입니다.\n");

// 혹은 error_log를 이용하여 표준 에러에 쓰기
error_log("이건 에러 로그를 통한 메시지입니다.", 4);
?>
```
위 코드 실행 결과, 표준 에러 스트림에 메시지가 출력됩니다.

## Deep Dive (심화 탐구)
예전에는 `fopen('php://stderr', 'w')` 를 사용하여 표준 에러 파일 핸들을 수동으로 열었습니다. 이제는 `STDERR` 상수로 직접 작성할 수 있으며, `error_log` 함수에 4번째 옵션을 주어 표준 에러로 로깅합니다. 이 상수와 함수는 내부적으로 시스템의 에러 핸들링과 연동되어 작동합니다.

## See Also (참고자료)
- PHP 공식 문서: [fwrite](https://www.php.net/manual/function.fwrite)
- PHP 공식 문서: [error_log](https://www.php.net/manual/function.error-log)
- PHP 에러 핸들링에 대한 추가 정보: [set_error_handler](https://www.php.net/manual/function.set-error-handler)
- 표준 출력과 에러 스트림에 대한 차이점 설명: [Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
