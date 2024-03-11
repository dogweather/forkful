---
date: 2024-01-20 17:55:03.707697-07:00
description: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uD30C\uC77C \uB0B4\
  \uC6A9\uC744 \uBB38\uC790 \uB370\uC774\uD130\uB85C \uC77D\uC5B4\uC624\uB294 \uAC83\
  \uC785\uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574 \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uC124\uC815, \uB370\uC774\uD130 \uB610\uB294 \uC790\uC6D0\uC744 \uC77D\uACE0\
  \ \uCC98\uB9AC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.304336-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uD30C\uC77C \uB0B4\uC6A9\
  \uC744 \uBB38\uC790 \uB370\uC774\uD130\uB85C \uC77D\uC5B4\uC624\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574 \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC124\
  \uC815, \uB370\uC774\uD130 \uB610\uB294 \uC790\uC6D0\uC744 \uC77D\uACE0 \uCC98\uB9AC\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 읽기는 파일 내용을 문자 데이터로 읽어오는 것입니다. 이를 통해 프로그래머는 설정, 데이터 또는 자원을 읽고 처리할 수 있습니다.

## How to: (방법)
PHP는 텍스트 파일을 쉽게 읽을 수 있는 여러 함수를 제공합니다. `file_get_contents`와 `fopen`/`fgets`/`fclose` 조합을 소개합니다.

`file_get_contents` 사용 예:
```php
<?php
$content = file_get_contents("example.txt");
echo $content;
?>
```
출력 예:
```
안녕하세요, 파일의 내용입니다!
```

`fopen`과 `fgets`, `fclose` 사용 예:
```php
<?php
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
} else {
    echo "파일을 열 수 없습니다.";
}
?>
```
출력 예:
```
안녕하세요, 파일의 내용입니다!
나는 두 번째 줄입니다.
```

## Deep Dive (심층 분석)
텍스트 파일 읽기는 초기 프로그래밍 시절부터 있었습니다. 빠른 읽기를 위한 `file_get_contents`와 제어가 필요할 때 `fopen`, `fgets`, `fclose`를 사용합니다. `file_get_contents`는 전체 파일을 한 번에 읽지만, `fopen`과 `fgets`는 한 줄씩 읽어 메모리 효율성이 높다.

또한, 파일 시스템 함수 사용 시 파일 경로 주의가 필요합니다. 예를 들어 대상 파일의 접근 권한이 없거나 파일이 존재하지 않으면 PHP 에러가 발생합니다. 오류 처리를 위해 `file_exists`와 `is_readable` 같은 함수를 사용하세요.

## See Also (참고 자료)
- PHP Official Documentation - Filesystem Functions: https://www.php.net/manual/en/ref.filesystem.php
- `file_get_contents` Documentation: https://www.php.net/manual/en/function.file-get-contents.php
- `fopen` Documentation: https://www.php.net/manual/en/function.fopen.php
- `fgets` Documentation: https://www.php.net/manual/en/function.fgets.php
- `fclose` Documentation: https://www.php.net/manual/en/function.fclose.php
- PHP Error Handling: https://www.php.net/manual/en/book.errorfunc.php
