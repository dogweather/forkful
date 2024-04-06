---
date: 2024-01-20 17:56:42.263052-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC2E4\uD589 \uACB0\
  \uACFC."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.075025-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## How to: (어떻게 하나요?)
```PHP
<?php
// PHP에서 명령줄 인수를 어떻게 다루는지 보여주는 예제입니다.

if ($argc > 1) {
    echo "첫 번째 인수: " . $argv[1] . "\n";
} else {
    echo "인수가 없습니다.\n";
}
?>
```

실행 결과:

```
$ php script.php
인수가 없습니다.

$ php script.php 안녕하세요
첫 번째 인수: 안녕하세요
```

## Deep Dive (깊은 다이빙)
명령줄 인수를 읽는 기능은 CLI(Command Line Interface) 기반 스크립트에서 오래 전부터 사용되어 왔습니다. `$argv`는 인수의 배열을 가지고 있고, `$argc` 는 인수의 수를 나타냅니다. 대체할 수 있는 방법으로는 `getopt()` 함수가 있지만, 이 함수는 옵션과 그 값을 분석할 때 더 적합합니다. `$argv`와 `$argc`를 사용하면 단순하게 위치 기반으로 인수를 처리할 수 있습니다. 구현 상세로는, `$argv[0]`은 스크립트의 이름이며, `$argv[1]`부터 시작하는 배열 인덱스는 각각의 인수에 대응합니다.

## See Also (더 보기)
- PHP 공식 문서의 `$_SERVER` 배열: https://www.php.net/manual/en/reserved.variables.server.php
- 공식 PHP CLI 문서: https://www.php.net/manual/en/features.commandline.php
- `getopt()` 함수에 대한 PHP 문서: https://www.php.net/manual/en/function.getopt.php
