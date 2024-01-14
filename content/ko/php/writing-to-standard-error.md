---
title:                "PHP: 표준 오류에 쓰는 것"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 코드를 표준 오류에 작성하는가
표준 오류에 코드를 작성하는 것은 디버깅 과정에서 매우 중요합니다. 이는 코드가 오류를 생성할 때 오류 메시지가 표시되어 실제 문제를 파악하고 해결하는 데 도움이 되기 때문입니다.

## 작성하는 방법
```PHP
// 예시 코드
<?php
    $x = 10;
    $y = 0;

    // 오류를 발생시키는 부분
    $z = $x / $y;

    // 표준 오류에 메시지 출력
    fwrite(STDERR, "오류 발생: 0으로 나눌 수 없습니다.");
```
다음과 같이 `fwrite()` 함수를 사용하여 표준 오류에 메시지를 출력할 수 있습니다. 이를 통해 오류가 발생한 위치를 확인할 수 있으며, 필요한 경우 오류 메시지를 수정할 수 있습니다.

### 예시 출력
```
오류 발생: 0으로 나눌 수 없습니다.
```

## 깊이 파고들기
표준 오류에 대해 더 깊이 알아보겠습니다. 표준 오류는 `fwrite()` 함수를 사용하여 출력할 수 있으며, `STDERR`이라는 파일 포인터를 통해 접근할 수 있습니다. 이는 표준 입력(`STDIN`)과 마찬가지로 파일 포인터를 사용하여 데이터를 읽고 쓸 수 있는 것을 의미합니다.

# 더 알아보기
- [PHP 공식 문서 - fwrite() 함수](https://php.net/manual/en/function.fwrite.php)
- [PHP 공식 문서 - 표준 파일 스트림](https://php.net/manual/en/features.commandline.io-streams.php)
- [코드 속도를 향상시키는 디버깅 팁](https://wsschool.kr/php/phpconsole.php)