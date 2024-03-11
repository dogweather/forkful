---
date: 2024-01-26 01:07:39.011584-07:00
description: "\uB85C\uADF8(Logging)\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C \uCF54\uB4DC\
  \uC5D0 \uB300\uD55C \uC77C\uAE30\uC7A5\uC744 \uC720\uC9C0\uD558\uB294 \uAC83\uACFC\
  \ \uC720\uC0AC\uD569\uB2C8\uB2E4; \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\uC774 \uC2E4\
  \uD589\uB420 \uB54C \uBC1C\uC0DD\uD558\uB294 \uC774\uBCA4\uD2B8, \uC5D0\uB7EC \uBC0F\
  \ \uAE30\uD0C0 \uC911\uC694\uD55C \uB370\uC774\uD130 \uD3EC\uC778\uD2B8\uB97C \uAE30\
  \uB85D\uD558\uB294 \uD589\uC704\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC2DC\uC2A4\uD15C \uB0B4\uBD80\uC5D0\uC11C \uBB34\uC2A8 \uC77C\uC774\
  \ \uC77C\uC5B4\uB098\uB294\uC9C0 \uCD94\uC801\uD558\uACE0, \uBB38\uC81C\uB97C \uB514\
  \uBC84\uADF8\uD558\uBA70, \uB098\uC911\uC5D0\u2026"
lastmod: '2024-03-11T00:14:29.288485-06:00'
model: gpt-4-1106-preview
summary: "\uB85C\uADF8(Logging)\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C \uCF54\uB4DC\uC5D0\
  \ \uB300\uD55C \uC77C\uAE30\uC7A5\uC744 \uC720\uC9C0\uD558\uB294 \uAC83\uACFC \uC720\
  \uC0AC\uD569\uB2C8\uB2E4; \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\uC774 \uC2E4\uD589\
  \uB420 \uB54C \uBC1C\uC0DD\uD558\uB294 \uC774\uBCA4\uD2B8, \uC5D0\uB7EC \uBC0F \uAE30\
  \uD0C0 \uC911\uC694\uD55C \uB370\uC774\uD130 \uD3EC\uC778\uD2B8\uB97C \uAE30\uB85D\
  \uD558\uB294 \uD589\uC704\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC2DC\uC2A4\uD15C \uB0B4\uBD80\uC5D0\uC11C \uBB34\uC2A8 \uC77C\uC774 \uC77C\
  \uC5B4\uB098\uB294\uC9C0 \uCD94\uC801\uD558\uACE0, \uBB38\uC81C\uB97C \uB514\uBC84\
  \uADF8\uD558\uBA70, \uB098\uC911\uC5D0\u2026"
title: "\uB85C\uAE45"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하나?

로그(Logging)는 기본적으로 코드에 대한 일기장을 유지하는 것과 유사합니다; 어플리케이션이 실행될 때 발생하는 이벤트, 에러 및 기타 중요한 데이터 포인트를 기록하는 행위입니다. 프로그래머들은 시스템 내부에서 무슨 일이 일어나는지 추적하고, 문제를 디버그하며, 나중에 분석하거나 준수 사항을 위한 감사 추적을 유지하기 위해 이를 수행합니다.

## 어떻게 사용하나:

PHP는 사용하기 쉬운 내장 에러 로깅 기능을 제공합니다. `error_log()`를 코드에 넣어서 메시지를 서버 로그로 보낼 수 있습니다. 또한 특정 파일로 기록하도록 커스터마이즈할 수도 있습니다.

```php
<?php
// 간단한 정보 메시지 로깅
error_log("This is an info log entry.");

// 에러 메시지 로깅
error_log("This is an error log entry.", 0);

// 지정된 파일에 로깅
file_put_contents('/path/to/your/custom.log', "A custom log entry.\n", FILE_APPEND);

// Monolog를 사용한 구조화 된 로깅
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// 로거 생성
$logger = new Logger('name');
// 이제 핸들러를 추가하십시오
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// 이제 로거를 사용할 수 있습니다
$logger->warning('This is a warning log!');
$logger->error('This is an error log!');
?>
```

이렇게 하면 로그가 서버 로그나 지정된 파일에 일반 텍스트 형식으로 출력됩니다.

## 심층 분석:

역사적으로 PHP 개발자들은 `error_log()` 함수나 Apache/Nginx 로그를 사용하여 문제를 파악했지만, 일반 텍스트 파일을 파싱해야 하고 필터링이나 정렬이 쉽지 않아 혼란스러울 수 있습니다. 그래서 Monolog 같은 로깅 라이브러리가 등장하여 PHP에서 구조화된 로깅의 시대를 열었습니다. 이러한 솔루션은 다중 로깅 채널, 심각도 레벨, 서식 있는 출력(예: 프로그래매틱하게 파싱하기에 꿈 같은 JSON) 등을 제공하여 더 나은 제어를 가능하게 해줍니다.

Monolog에 대한 대안으로는 Log4php, KLogger, Apache의 Log4php가 있습니다. 구현 측면에서, 견고한 로깅은 단순히 어디에나 데이터를 버리는 것이 아니라, 로그 회전, 보관 전략 및 모니터링 도구와의 통합을 고려하여 진정으로 유용해야 합니다.

로깅 라이브러리간의 상호운용성과 로깅 메커니즘에 대한 일관된 접근 방식을 보장하기 위해 [PSR-3 로거 인터페이스](https://www.php-fig.org/psr/psr-3/)를 염두에 두어야 합니다.

## 참고 자료:

- [Monolog GitHub 저장소](https://github.com/Seldaek/monolog)
- [PSR-3 로거 인터페이스 사양](https://www.php-fig.org/psr/psr-3/)
- [PHP 에러 로그 문서](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: PHP용 간단한 로깅 클래스](https://github.com/katzgrau/KLogger)
- [Log4php: PHP용 다재다능한 로깅 프레임워크](https://logging.apache.org/log4php/)

내장 함수로 시작하되, 더 유지보수 가능하고 확장 가능한 접근 방식을 위해 Monolog 같은 라이브러리에 익숙해지는 데 시간을 투자하는 것을 고려해보세요. 로깅에 행운을 빕니다!
