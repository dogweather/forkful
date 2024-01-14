---
title:                "PHP: 컴퓨터 프로그래밍에서 커맨드 라인 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 커맨드 라인 인수 읽기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

명령 줄 인수를 읽는 방법에 대해 이야기해보자. 이것은 PHP 프로그래밍에서 매우 유용하며, 개발자가 프로그램을 실행할 때 명령 줄에서 추가로 전달할 수 있는 매개 변수를 사용할 수 있게 해준다. 이 게시물에서는 이러한 기능의 중요성과 사용법을 알아보자.

## 어떻게

명령 줄 인수를 읽는 것은 간단하지만 처리해야 할 몇 가지 사항이 있다. 우선, `$argc`와 `$argv`를 사용해 해당 매개 변수를 어떻게 가져오는지 알아야 한다. 그리고 그것들을 사용하여 필요한 작업을 할 수 있다.

```PHP
<?php
// 인수 개수
echo "인수 개수: " . $argc . "\n";

// 인수 목록
echo "인수 목록: ";
print_r($argv);
```

예를 들어, `$argv[0]`는 스크립트의 이름이고 `$argv[1]`부터는 전달된 인수들이다. 이를 기반으로 조건문을 사용하여 다양한 작업을 수행할 수 있다.

```PHP
<?php
// 인수 개수가 2개 이상이면 실행
if ($argc > 1) {
    // 첫 번째 인수 출력
    echo "안녕하세요, " . $argv[1] . "님!";
} else {
    // 인수가 전달되지 않았을 때의 처리
    echo "이름이 뭐에요?";
}
```

위의 예시 코드를 실행하여 스크립트 파일 이름과 다양한 이름을 전달해보면서 결과를 확인해보자.

## 딥 다이브

명령 줄 인수를 읽는 데는 더 많은 부분이 있지만, 이러한 기본 사항을 알고 있다면 어렵지 않게 다룰 수 있다. 더 깊이 들어가는 것은 우리의 목적과 범위를 벗어나기 때문에 이 정도만 소개하도록 하겠다. 다만, 코드를 작성할 때 유용하게 사용할 수 있는 `getopt()` 함수가 있다는 것만 알아두자.

## 참고 자료

- [PHP 매뉴얼](https://www.php.net/manual/en/reserved.variables.argc.php)
- [PHP Manual - Command Line Usage](https://www.php.net/manual/en/features.commandline.usage.php)
- [PHP Manual - getopt()](https://www.php.net/manual/en/function.getopt.php)
- [Command Line Arguments in PHP](https://www.geeksforgeeks.org/command-line-arguments-in-php/)
- [Reading command line arguments in PHP](https://medium.com/@rossbulat/reading-command-line-arguments-in-php-7-5c2c2c7a5fc8)

## 참조

- 이 게시물은 [PHP: The Right Way](https://www.phptherightway.com/)의 [Command Line Arguments](https://www.phptherightway.com/#command_line_arguments) 섹션을 참고하여 작성되었습니다.