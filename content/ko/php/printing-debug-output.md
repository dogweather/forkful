---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## 이게 무엇이며 왜 쓰는가?

디버그 출력이란 프로그램이 실행되는 동안 정보를 출력하는 것 입니다. 프로그래머들은 이를 이용해 코드 동작이 문제 없는지 확인하고, 버그를 찾는데 사용합니다.

## 방법은?

PHP에서 디버그 출력을 쓸 수 있는 가장 간단한 방법은 'echo' 또는 'print' 구문을 사용하는 것입니다.

```PHP
<?php
$x = 5;
echo "The value of x is: " . $x;
?>
```

위 코드를 실행하면, 다음과 같은 출력을 볼 수 있습니다.

```
The value of x is: 5
```

## 깊은 이해

디버그 출력은 오래 전부터 프로그래밍의 기본적인 검증 방법 중 하나로 사용되어 왔습니다. PHP에서는 'echo'나 'print' 외에도 'var_dump()'나 'print_r()' 같은 함수를 사용할 수 있어 보다 상세한 디버그 정보를 확인할 수 있습니다.

```PHP
<?php
$arr = array('a', 'b', 'c');
var_dump($arr);
?>
```

위 코드를 실행하면, 배열의 모든 요소와 해당 값이 출력됩니다.

또 다른 대안으로는 오류 리포팅 기능을 이용하는 방법이 있습니다. 이를 사용하면 PHP 자체적으로 발생하는 오류와 경고를 자세히 알아낼 수 있습니다.

```PHP
<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

x = 5
?>
```

위 코드를 실행하면, 세미콜론이 빠진 오류를 자동으로 감지하고 알려줍니다.  

## 관련 자료

더 많은 정보를 원하신다면 아래 링크를 참조하세요.

- PHP Manual: Error Reporting - https://www.php.net/manual/en/function.error-reporting.php
- PHP Debugging Basics - http://php.net/manual/en/debugger.php
- Using 'print_r' and 'var_dump' - https://php.net/manual/function.print-r.php