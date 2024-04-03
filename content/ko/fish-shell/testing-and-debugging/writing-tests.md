---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:06.254903-07:00
description: "\uBC29\uBC95: Fish\uB294 \uB2E4\uB978 \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uD658\uACBD\uCC98\uB7FC \uB0B4\uC7A5\uB41C \uD14C\uC2A4\uD305 \uD504\uB808\uC784\
  \uC6CC\uD06C\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\
  \uB7EC\uB098, \uD568\uC218\uC758 \uB3D9\uC791\uC744 \uD655\uC778\uD558\uAE30 \uC704\
  \uD574 \uB2E8\uC5B8\uBB38\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC04\uB2E8\uD55C \uD14C\
  \uC2A4\uD2B8 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uB610\uD55C, \uBCF4\uB2E4 \uC885\uD569\uC801\uC778 \uD14C\uC2A4\uD305\
  \ \uC2A4\uC704\uD2B8\uB97C \uC704\uD574 `fishtape`\uC640 \uAC19\uC740 \uD0C0\uC0AC\
  \ \uB3C4\uAD6C\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.860772-06:00'
model: gpt-4-0125-preview
summary: "Fish\uB294 \uB2E4\uB978 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uCC98\
  \uB7FC \uB0B4\uC7A5\uB41C \uD14C\uC2A4\uD305 \uD504\uB808\uC784\uC6CC\uD06C\uB97C\
  \ \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Fish는 다른 프로그래밍 환경처럼 내장된 테스팅 프레임워크를 가지고 있지 않습니다. 그러나, 함수의 동작을 확인하기 위해 단언문을 사용하는 간단한 테스트 스크립트를 작성할 수 있습니다. 또한, 보다 종합적인 테스팅 스위트를 위해 `fishtape`와 같은 타사 도구를 활용할 수 있습니다.

### 예시 1: 기본 테스트 스크립트
두 숫자의 합을 계산하는 Fish에서의 기본 함수로 시작해 봅시다:

```fish
function add --description '두 숫자 더하기'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

이 함수에 대한 기본 테스트 스크립트는 다음과 같이 작성할 수 있습니다:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add 통과"
    else
        echo "test_add 실패"
    end
end

test_add
```

이 스크립트를 실행하면 다음과 같은 출력이 나옵니다:

```
test_add 통과
```

### 예시 2: Fishtape 사용하기
보다 견고한 테스팅 솔루션을 위해, Fish용 TAP 생산 테스트 러너인 `fishtape`을 사용할 수 있습니다.

먼저, 아직 설치하지 않았다면 `fishtape`을 설치합니다:

```fish
fisher install jorgebucaran/fishtape
```

다음으로, `add` 함수에 대한 테스트 파일을 만듭니다. 예: `add_test.fish`:

```fish
test "3과 4를 더하면 7이 나온다"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

테스트를 실행하려면 다음 명령을 사용하세요:

```fish
fishtape add_test.fish
```

표본 출력은 다음과 같아 보일 것입니다:

```
TAP version 13
# 3과 4를 더하면 7이 나온다
ok 1 - test_add 통과
```

이는 테스트가 성공적으로 통과했음을 알려줍니다. `fishtape`은 보다 상세한 테스트를 구조화하고 정보적인 출력을 제공하여, Fish 스크립트에 대한 디버깅을 용이하게 하고 포괄적인 테스트 커버리지를 용이하게 합니다.
