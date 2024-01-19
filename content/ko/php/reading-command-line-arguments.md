---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

커맨드 라인 인자 읽기는 사용자가 프로그램을 실행할 때 제공하는 값을 읽는 것입니다. 이를 통해 프로그래머들은 유동적이고 맞춤형 부분의 프로그램을 개발하게 됩니다.

## 어떻게 사용하나:

PHP에서 CLI 인자는 전역 배열 `$_SERVER['argv']`에 저장됩니다. 이 배열의 첫 번째 요소는 스크립트 이름이고, 나머지 요소는 순서대로 커맨드 라인 인자입니다.
클릭하세요.

```PHP

<?php
    // index.php 파일이라고 가정하자.
    var_dump($_SERVER['argv']);
?>

```

터미널에서 스크립트를 이렇게 실행하면

```
$ php index.php hello world
```

아래와 같은 결과가 표시됩니다:

```
array(3) {
  [0]=>
  string(9) "index.php"
  [1]=>
  string(5) "hello"
  [2]=>
  string(5) "world"
}
```

## 디프다이브:

커맨드 라인 인자의 사용법은 스크립트 언어의 다양한 시대를 거쳐 변해왔습니다. 현대의 PHP에서는 `$_SERVER['argv']` 배열을 통해 커맨드 라인 인자에 접근합니다.

대안으로, PHP는 `getopt()`라는 함수도 제공합니다. 이 함수는 옵션 이름과 함께 인자를 제공하는 더 복잡한 사용 사례를 지원합니다. 

한편, 커맨드 라인 인자를 직접 사용하는 방식 대신 표준 입력(STDIN) 또는 파일 입력을 통해 사용자로부터 데이터를 읽는 방법도 있습니다.

## 참고 자료:

- [PHP Manual](https://www.php.net/manual/en/reserved.variables.argv.php) - $_SERVER['argv'] 서툴 및 사용 방법에 대한 자세한 정보
- [PHP: getopt - Manual](https://www.php.net/manual/en/function.getopt.php) - PHP의 getopt 함수에 대한 자세한 정보
- [PHP Manual](https://www.php.net/manual/en/features.commandline.io-streams.php) - PHP에서 STDIN과 STDOUT에 대한 자세한 정보