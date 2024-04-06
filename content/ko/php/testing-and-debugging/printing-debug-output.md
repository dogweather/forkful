---
date: 2024-01-20 17:53:15.003968-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C \uB514\
  \uBC84\uADF8 \uBA54\uC2DC\uC9C0\uB97C \uCD9C\uB825\uD558\uB294 \uAC00\uC7A5 \uB2E8\
  \uC21C\uD55C \uBC29\uBC95\uC740 `echo` \uB098 `print` \uBA85\uB839\uC5B4\uB97C \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.060321-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C \uB514\uBC84\uADF8\
  \ \uBA54\uC2DC\uC9C0\uB97C \uCD9C\uB825\uD558\uB294 \uAC00\uC7A5 \uB2E8\uC21C\uD55C\
  \ \uBC29\uBC95\uC740 `echo` \uB098 `print` \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\
  \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

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
