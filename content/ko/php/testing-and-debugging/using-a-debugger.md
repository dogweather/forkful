---
date: 2024-01-26 03:50:59.810066-07:00
description: "PHP\uC5D0\uB294 Xdebug\uB77C\uB294 \uB300\uD654\uD615 \uB514\uBC84\uAC70\
  \uAC00 \uC81C\uACF5\uB429\uB2C8\uB2E4. \uC0AC\uC6A9 \uBC29\uBC95\uC740 \uB2E4\uC74C\
  \uACFC \uAC19\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, Xdebug\uAC00 \uC124\uCE58\uB418\uC5B4\
  \ \uC788\uACE0 `php.ini` \uD30C\uC77C\uC5D0 \uAD6C\uC131\uB418\uC5B4 \uC788\uB294\
  \uC9C0 \uD655\uC778\uD569\uB2C8\uB2E4: ``` zend_extension=/usr/local/lib/php/extensions/no-\u2026"
lastmod: '2024-03-13T22:44:55.363317-06:00'
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uB294 Xdebug\uB77C\uB294 \uB300\uD654\uD615 \uB514\uBC84\uAC70\
  \uAC00 \uC81C\uACF5\uB429\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 사용 방법:
PHP에는 Xdebug라는 대화형 디버거가 제공됩니다. 사용 방법은 다음과 같습니다.

먼저, Xdebug가 설치되어 있고 `php.ini` 파일에 구성되어 있는지 확인합니다:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

다음으로, 버그가 있는 간단한 PHP 스크립트를 작성합니다:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // 이런! 이 부분은 빼기가 아니라 더하기여야 합니다
}

$result = add(1, 2);
echo "결과는: $result"; // 출력 결과는 -1이 아니라 3이어야 합니다
```

PhpStorm과 같은 IDE를 사용하여, 줄 번호 옆을 클릭하여 중단점을 설정합니다. 디버거를 실행하고 실행을 단계별로 진행하면서 변수가 어떻게 변경되는지 관찰합니다. `add` 함수를 건너뛸 때, `$result`가 예상치 못하게 -1이 되는 것을 알게 될 것입니다.

## 깊이 들여다보기:
역사적으로 PHP는 작은 스크립트에 주로 사용되었고, 디버깅은 코드 전반에 `var_dump()`와 `print_r()` 문장을 추가하는 것이었습니다. 시간이 지나면서 PHP가 웹 개발에서 핵심적 역할을 하게 되면서 Xdebug와 Zend Debugger와 같은 더 정교한 도구들이 사용되기 시작했습니다.

Xdebug의 대안으로는 pcov와 phpdbg가 있습니다. 이들은 다양한 기능을 제공하지만 Xdebug만큼 완벽한 기능을 제공하지 않을 수 있습니다. phpdbg는 PHP 5.6부터 PHP와 함께 배포되는 가벼운, PHP 전용 디버거이고, pcov는 코드 커버리지 드라이버입니다.

디버거를 구현할 때, 보안 취약점을 노출시키고 성능을 저하시킬 수 있으므로 프로덕션 서버에서 디버거를 켜두어서는 안된다는 점을 기억하세요.

## 또한 보십시오:
- [Xdebug 문서](https://xdebug.org/docs/)
- [PhpStorm 디버깅 가이드](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net의 phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [GitHub의 pcov](https://github.com/krakjoe/pcov)
