---
title:    "PHP: 컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

명령줄 인자를 읽는 방법을 배우는 것은 PHP 프로그래머로서 매우 유용합니다. 명령줄 인자를 사용하면 사용자로부터 입력을 받을 수 있으며, 동시에 미리 정의된 기능을 호출할 수도 있습니다.

## 방법

PHP에서는 `$argv` 변수를 사용하여 명령줄 인자를 읽을 수 있습니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

```PHP
// Example.php 파일에서 실행된 명령줄 인자 읽기

<?php
$argument = $argv[1];
echo "입력된 인자는 " . $argument . " 입니다.";
?>
```

위의 예제에서 `Example.php` 파일을 실행할 때 인자를 함께 입력하면 입력한 인자를 출력해줍니다. 예를 들어, `php Example.php "안녕하세요"`라고 입력하면 `입력된 인자는 안녕하세요 입니다.`라는 결과가 출력됩니다.

## 딥 다이브

PHP에서는 `getopt()` 함수를 사용하여 더욱 복잡한 명령줄 인자를 처리할 수 있습니다. 이 함수는 이름을 가지는 인자를 받아와 배열로 반환해줍니다. 다음은 `getopt()` 함수를 사용하는 예제입니다.

```PHP
// Example.php 파일에서 실행된 명령줄 인자 읽기

<?php
$options = getopt("a:b:c:");
foreach ($options as $key => $value) {
    echo "키: " . $key . " 값: " . $value . PHP_EOL;
}
?>
```

위의 예제에서는 `a`, `b`, `c`라는 이름을 가지는 인자를 받아와 해당하는 값을 출력해줍니다. 예를 들어, `php Example.php -a 1 -b 2 -c 3`라고 입력하면 `키: a 값: 1`, `키: b 값: 2`, `키: c 값: 3`이라는 결과가 출력됩니다.

## 참고 자료

- [PHP 공식 문서 - 명령줄 인자](https://www.php.net/manual/en/features.commandline.php)
- [PHP 공식 문서 - getopt() 함수](https://www.php.net/manual/en/function.getopt.php)
- [3분 PHP - 명령줄 인자 읽기](https://youtu.be/vNntHoLSDhM)