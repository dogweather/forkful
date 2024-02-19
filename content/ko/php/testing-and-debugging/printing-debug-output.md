---
aliases:
- /ko/php/printing-debug-output/
date: 2024-01-20 17:53:15.003968-07:00
description: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uAC00 \uC5B4\uB5BB\
  \uAC8C \uB3D9\uC791\uD558\uB294\uC9C0 \uC774\uD574\uD558\uB824\uACE0 \uD560 \uB54C\
  \ \uC4F0\uB294 \uBA54\uC2DC\uC9C0\uC785\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC740\
  \ \uBC84\uADF8\uB97C \uCC3E\uACE0, \uBCC0\uC218\uC758 \uC0C1\uD0DC\uB97C \uD655\uC778\
  \uD558\uACE0, \uC2E4\uD589 \uD750\uB984\uC744 \uCD94\uC801\uD558\uAE30 \uC704\uD574\
  \ \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.356226
model: gpt-4-1106-preview
summary: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uAC00 \uC5B4\uB5BB\uAC8C\
  \ \uB3D9\uC791\uD558\uB294\uC9C0 \uC774\uD574\uD558\uB824\uACE0 \uD560 \uB54C \uC4F0\
  \uB294 \uBA54\uC2DC\uC9C0\uC785\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC740 \uBC84\
  \uADF8\uB97C \uCC3E\uACE0, \uBCC0\uC218\uC758 \uC0C1\uD0DC\uB97C \uD655\uC778\uD558\
  \uACE0, \uC2E4\uD589 \uD750\uB984\uC744 \uCD94\uC801\uD558\uAE30 \uC704\uD574 \uC774\
  \uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하는가?)
디버그 출력은 코드가 어떻게 동작하는지 이해하려고 할 때 쓰는 메시지입니다. 개발자들은 버그를 찾고, 변수의 상태를 확인하고, 실행 흐름을 추적하기 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
PHP에서 디버그 메시지를 출력하는 가장 단순한 방법은 `echo` 나 `print` 명령어를 사용하는 것입니다.

```PHP
<?php
$variable = 'Hello, World!';
echo $variable; // 화면에 변수 내용을 출력
?>
```
**Sample Output:**
```
Hello, World!
```

때로는 배열이나 객체 같은 복잡한 데이터 타입을 출력해야 할 때가 있죠. 이때는 `print_r()` 또는 `var_dump()`를 사용합니다.

```PHP
<?php
$array = ['apple', 'banana', 'cherry'];
print_r($array);

// 또는 좀 더 상세한 정보를 원한다면

var_dump($array);
?>
```
**Sample Output:**
```
Array
(
    [0] => apple
    [1] => banana
    [2] => cherry
)
```

## Deep Dive (심층 분석)
에코(echo)는 PHP 초기 버전부터 사용해왔던 간단한 출력 구문입니다. `print`와 거의 유사하지만, 일부 성능상 차이가 있어 `echo`가 더 널리 사용됩니다. `print_r()`는 배열 같은 자료 구조를 읽기 쉽게 출력할 때 사용하고, `var_dump()`는 변수의 데이터 타입과 값을 출력해주는데, 디버깅할 때 유용합니다. 

Xdebug과 같은 전문 디버거도 있지만, 간단한 디버깅이나 로깅을 위해서는 `error_log()` 함수를 사용하여 서버 로그에 메시지를 남길 수도 있습니다. 이는 출력이 사용자에게 보이지 않기를 바랄 때 유용합니다.

## See Also (더 보기)
- [PHP official documentation on echo](https://www.php.net/manual/en/function.echo.php)
- [PHP official documentation on print_r](https://www.php.net/manual/en/function.print-r.php)
- [PHP official documentation on var_dump](https://www.php.net/manual/en/function.var-dump.php)
- [Xdebug - Debugger and Profiler Tool for PHP](https://xdebug.org/)
