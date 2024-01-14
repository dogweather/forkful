---
title:    "PHP: 컴퓨터 프로그래밍에서의 명령행 인수 읽기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜?

PHP 프로그래밍을 배우는 독자들은 일반적으로 명령 줄 인수를 읽는 방법을 배우고 싶어할 것입니다. 이 작업을 마스터하면 PHP 프로그램을 작성하고 실행하는 데 있어서 더 많은 유용한 기능을 발견할 수 있습니다.

## 어떻게?

명령 줄 인수를 읽는 가장 간단한 방법은 `argv` 전역 변수를 사용하는 것입니다. 다음의 예시 코드를 참고해주세요:

```PHP
<?php

// 스크립트 실행 시 사용된 명령 줄 인수를 모두 출력합니다.
foreach ($argv as $arg) {
    echo $arg . "\n";
}
```

위 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다:

```
php script.php arg1 arg2 arg3
```

```
script.php
arg1
arg2
arg3
```

만약 명령 줄 인수를 특정 변수에 저장하고 싶다면, 아래와 같이 할 수 있습니다:

```PHP
<?php

// 스크립트 실행 시 전달된 첫 번째 인수를 $first 변수에 저장합니다.
$first = $argv[1];

// 인수를 출력합니다.
echo $first;
```

출력 결과:

```
php script.php arg1
```

```
arg1
```

## 심층 탐구

프로그래밍에서 명령 줄 인수는 매우 중요한 요소입니다. 그러므로 더 많은 심층적인 정보를 알고있다는 것은 매우 유용할 것입니다. 명령 줄 인수를 더 잘 이해하기 위해 다음의 자료를 참고해보세요:

- [PHP 쉘 스크립트 작성하기](https://www.php.net/manual/kr/features.commandline.php)
- [MVC 패턴 및 명령 줄 인수 사용하기](https://medium.com/@gscheibel/mvc-pattern-using-command-line-arguments-da31336a9c8)
- [CLI로 서버 관리하기](https://serversforhackers.com/c/running-php-cli-scripts-with-bash-scripts)

## 또 참고해보세요

- [PHP 공식 매뉴얼](https://www.php.net/manual/kr/)
- [튜토리얼 포스트: PHP 프로그래밍 입문하기](https://www.udemy.com/course/the-complete-php-bootcamp/)
- [이제부터 배워나가면서 해보세요](https://www.w3schools.com/php/)