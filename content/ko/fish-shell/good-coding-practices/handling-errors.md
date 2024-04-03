---
date: 2024-01-26 00:52:50.157928-07:00
description: "\uC5D0\uB7EC \uD578\uB4E4\uB9C1\uC740 \uC2A4\uD06C\uB9BD\uD2B8\uAC00\
  \ \uC608\uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC744 \uC6B0\uC544\uD558\uAC8C \uCC98\
  \uB9AC\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uC0AC\
  \uC6A9\uC790\uC758 \uBA38\uB9AC\uCE74\uB77D\uC744 \uD558\uC597\uAC8C \uD560 \uC815\
  \uB3C4\uB85C \uC2E4\uD328\uB97C \uAD00\uB9AC\uD558\uAE30 \uC704\uD574 \uC774\uB97C\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.866879-06:00'
model: gpt-4-1106-preview
summary: "\uC5D0\uB7EC \uD578\uB4E4\uB9C1\uC740 \uC2A4\uD06C\uB9BD\uD2B8\uAC00 \uC608\
  \uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC744 \uC6B0\uC544\uD558\uAC8C \uCC98\uB9AC\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 어떻게:
Fish에서 에러를 잡기 위해서는 `status` 명령어와 조건문에 의존하십시오. 예를 들어 `ping`이 실패했다면, 그것을 감지하는 방법은 다음과 같습니다:

```fish
ping -c 1 example.com
if not status is-success
    echo "Something fishy happened with the ping."
end
```

`ping`이 실패했을 때의 예제 출력:

```
Something fishy happened with the ping.
```

특정 에러 코드를 처리하려면 `status --is`를 사용하십시오:

```fish
false
if status --is 1
    echo "Caught an error with code 1."
end
```

샘플 출력:
```
Caught an error with code 1.
```

더 견고한 접근 방식을 고려한다면, 함수를 사용하는 것이 좋습니다:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping failed with status $status"
        return 1
    end
end

try_ping
```

## 심층 분석
Fish의 에러 핸들링은 높은 수준의 언어에서 알려진 `try/catch` 패러다임과 일치하지 않습니다. 대신, `status` 명령어에 의해 제공되는 직관적인 종료 상태를 가지고 있습니다.

역사적으로 유닉스와 유사한 시스템에서, `0`은 성공을 의미하는 종료 상태이고, 이와 달리 비-0 값은 에러를 의미하며, 일반적으로 다양한 실패의 이유를 반영합니다. 이 관습은 대부분의 커맨드 라인 유틸리티에 의해 사용되므로, 이에 의해 Fish 자체에 의해 사용됩니다.

Fish에서 `status` 검사의 대안에는 다른 쉘에서 `trap`을 통한 시그널 핸들링이 있지만, Fish는 부작용이 덜하고 더 명확하기 때문에 명시적인 상태 검사를 선호합니다.

구현 면에서, Fish의 에러 핸들링은 그것의 논블로킹 특성과 명확한 문법 강조 덕분에 단순하면서도 강력합니다, 예시에서 볼 수 있듯이. 에러 코드는 함수와 잘 어우러져 모듈화되고 가독성 높은 에러 관리를 가능하게 합니다.

## 참조
- 조건문에 대한 Fish 문서: https://fishshell.com/docs/current/language.html#conditionals
- 에러 핸들링에 대한 Fish 튜토리얼: https://fishshell.com/docs/current/tutorial.html#error-handling
