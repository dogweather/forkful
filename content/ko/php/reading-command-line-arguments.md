---
title:                "PHP: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜?
커맨드 라인 인수를 읽는 것은 많은 사람들이 PHP 프로그래밍에서 자주하는 작업 중 하나입니다. 이 기능을 잘 파악하면 프로그래밍 작업의 효율성을 높일 수 있고 보다 유연한 코드를 작성하는 데 도움이 됩니다.

## 방법
커맨드 라인에서 제공된 인수를 PHP 코드로 읽어올 수 있습니다. 먼저, `$argv` 배열을 사용하여 전달된 인수를 읽는 것이 중요합니다. 다음은 예시 코드와 출력 결과입니다.

```PHP
$arguments = $argv[1]; // 커맨드 라인에서 전달된 인수를 저장합니다.
echo "전달된 인수는 {$arguments}입니다."; // 인수 출력
```

**출력 결과:**

```
전달된 인수는 예제입니다.
```

여러 개의 인수가 전달된 경우, `$argc` 변수를 사용하여 전달된 인수의 갯수를 확인할 수 있습니다.

이제부터는 실제로 커맨드 라인에서 실행하는 것과 비슷한 예제를 살펴보겠습니다.

```PHP
// 예제 파일 이름: command_arguments.php
$filename = $argv[0]; // 파일 이름 저장
$firstname = $argv[1]; // 이름을 입력할 변수
$lastname = $argv[2]; // 성을 입력할 변수

// 파일 이름 출력
echo "파일 이름: {$filename}\n";
// 이름과 성 출력
echo "{$firstname} {$lastname}님 안녕하세요?";
```

**커맨드 라인 명령어:**
```
php command_arguments.php Emma Stone
```

**출력 결과:**
```
파일 이름: command_arguments.php
Emma Stone님 안녕하세요?
```

## 딥 다이브
커맨드 라인 인수를 읽는 방법은 여러 가지가 있습니다. 예를 들어, `getopt()` 함수를 사용해 인수를 읽어올 수 있습니다. 이 함수는 배열로 결과를 반환하며 각 인수의 이름과 값을 포함합니다. 더 많은 정보는 PHP 공식 문서를 참고하시기 바랍니다.

## 참고 자료
- [PHP 공식 문서 (한국어)](https://www.php.net/manual/kr/index.php)
- [PHP 커뮤니티 사이트 (한국어)](https://www.phpinfo.co.kr)
- [PHP 커맨드 라인 인수 관련 문서 (영어)](https://www.php.net/manual/en/features.commandline.php)