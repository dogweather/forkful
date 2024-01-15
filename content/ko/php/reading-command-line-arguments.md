---
title:                "명령 줄 인수 읽기"
html_title:           "PHP: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

PHP 프로그래밍에서 커맨드 라인 인자를 읽는 것이 왜 중요한지에 대해 설명합니다. 이 기능을 사용하여 더욱 유연하게 프로그램을 작성하고 사용자에게 훨씬 더 많은 제어권을 주기 때문입니다.

## 사용 방법

```PHP
<?php
// 예제 1: 값으로 인자 읽기
$argument = $argv[1];
echo "입력한 인자는: $argument";
```

```PHP
<?php
// 예제 2: 플래그로 인자 읽기
$arguments = getopt("i:o:");
$input_file = $arguments['i'];
$output_file = $arguments['o'];
echo "입력 파일: $input_file\n출력 파일: $output_file";
```

```PHP
<?php
// 예제 3: 인자의 유효성 검사하기

if ($argc < 2) {
    echo "적어도 하나의 인자를 입력해주세요.";
    exit(1);
}

$file_name = $argv[1];
if (!file_exists($file_name)) {
    echo "입력한 파일이 존재하지 않습니다.";
    exit(1);
}

echo "파일이 성공적으로 로드되었습니다.";
```

## 딥 다이브

커맨드 라인 인자를 사용하는 방법은 다양합니다. 언어 차원에서는 `argc`와 `argv`를 통해 인자를 읽을 수 있지만, PHP에서는 이를 좀 더 유연하게 다룰 수 있는 `getopt()` 함수를 제공합니다. 또한, `getopt()`에서는 플래그 형식의 인자를 쉽게 읽을 수 있습니다. 더 나아가, 인자의 유효성을 검사하는 방법도 중요합니다. 위 예제에서는 `file_exists()`와 조건문을 사용하여 인자의 유효성을 검사할 수 있습니다.

## 참고

- [PHP 공식 문서 - 커맨드 라인 인자](https://www.php.net/manual/kr/reserved.variables.argv.php)
- [PHP 공식 문서 - getopt() 함수](https://www.php.net/manual/kr/function.getopt.php)
- [누구나 쉽게 이해하는 PHP 커맨드 라인 인자 사용법](https://web.archive.org/web/20150204024906/http://dablastor.hatenablog.jp/entry/20111231/1325309300)