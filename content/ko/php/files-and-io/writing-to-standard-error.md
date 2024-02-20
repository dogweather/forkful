---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:37.728923-07:00
description: "PHP\uC5D0\uC11C \uD45C\uC900 \uC5D0\uB7EC(stderr)\uB85C \uAE30\uB85D\
  \uD558\uB294 \uAC83\uC740 \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uB098 \uC9C4\uB2E8 \uC815\
  \uBCF4\uB97C \uD45C\uC900 \uCD9C\uB825(stdout)\uACFC \uBCC4\uB3C4\uB85C \uAD00\uB9AC\
  \uD558\uC5EC \uAC1C\uBC1C\uC790\uB4E4\uC774 \uB514\uBC84\uAE45\uACFC \uB85C\uAE45\
  \ \uBAA9\uC801\uC73C\uB85C \uCD9C\uB825 \uC2A4\uD2B8\uB9BC\uC744 \uB354 \uC798 \uAD00\
  \uB9AC\uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\
  \uD558\uC5EC \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uAC00 \uD504\uB85C\uADF8\uB7A8\uC758\
  \u2026"
lastmod: 2024-02-19 22:05:14.293040
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uC11C \uD45C\uC900 \uC5D0\uB7EC(stderr)\uB85C \uAE30\uB85D\uD558\
  \uB294 \uAC83\uC740 \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uB098 \uC9C4\uB2E8 \uC815\uBCF4\
  \uB97C \uD45C\uC900 \uCD9C\uB825(stdout)\uACFC \uBCC4\uB3C4\uB85C \uAD00\uB9AC\uD558\
  \uC5EC \uAC1C\uBC1C\uC790\uB4E4\uC774 \uB514\uBC84\uAE45\uACFC \uB85C\uAE45 \uBAA9\
  \uC801\uC73C\uB85C \uCD9C\uB825 \uC2A4\uD2B8\uB9BC\uC744 \uB354 \uC798 \uAD00\uB9AC\
  \uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uAC00 \uD504\uB85C\uADF8\uB7A8\uC758\u2026"
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

PHP에서 표준 에러(stderr)로 기록하는 것은 에러 메시지나 진단 정보를 표준 출력(stdout)과 별도로 관리하여 개발자들이 디버깅과 로깅 목적으로 출력 스트림을 더 잘 관리할 수 있게 하는 것을 말합니다. 프로그래머들은 이 기술을 사용하여 에러 메시지가 프로그램의 출력과 충돌하지 않도록 하여, 애플리케이션의 모니터링과 문제 해결을 더 쉽게 만듭니다.

## 방법:

PHP에서 stderr로 기록하는 것은 사전 정의된 상수 `STDERR`, 즉 에러 출력 스트림을 나타내는 것과 함께 `fwrite()` 함수를 사용하여 달성할 수 있습니다.

```php
<?php
// stderr에 간단한 메시지 기록하기.
fwrite(STDERR, "이것은 에러 메시지입니다.\n");
```

명령 줄에서 스크립트를 실행했을 때 샘플 출력:
```
이것은 에러 메시지입니다.
```

보다 실용적인 사용을 시연하기 위해 예상치 못한 데이터를 만나는 사용자 입력을 파싱하는 시나리오를 고려해 보세요:
```php
<?php
$input = 'unexpected data';

// 사용자 입력 처리 중 에러 시뮬레이션.
if ($input === 'unexpected data') {
    fwrite(STDERR, "에러: 예상치 못한 입력이 수신되었습니다.\n");
    exit(1); // 에러를 나타내는 비제로 값을 가진 상태에서 종료.
}
```

PHP의 내장 stderr 처리 기능은 일반적으로 충분하지만, 더 복잡한 애플리케이션을 다루거나 stderr 로깅을 외부 시스템과 통합하고 싶을 때는 Monolog와 같은 제3자 라이브러리가 강력한 동맹이 될 수 있습니다. Monolog는 파일, 소켓 등 다양한 대상(stdout 포함)에 대한 로깅을 처리할 수 있는 로깅 라이브러리입니다.

Monolog를 사용하여 stderr로 기록하기:

먼저, Composer를 통해 Monolog가 설치되어 있는지 확인하세요:
```
composer require monolog/monolog
```

그 다음, `php://stderr`를 대상으로 하는 `StreamHandler`를 사용하도록 Monolog를 구성할 수 있습니다:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// 로그 채널 생성
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// stderr에 로그 메시지 추가
$log->warning('이것은 경고 메시지입니다.');
```

위 코드는 외부 로그 모니터링 또는 상세한 로깅 구성이 필요한 애플리케이션에 특히 유용한 Monolog를 사용하여 stderr로 경고 메시지를 보내는 방법을 활용합니다.
