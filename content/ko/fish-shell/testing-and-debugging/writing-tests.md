---
aliases:
- /ko/fish-shell/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:06.254903-07:00
description: "Fish Shell\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uC790\uB3D9\uC73C\uB85C \uC2E4\uD589\uD558\
  \uC5EC \uC608\uC0C1 \uACB0\uACFC\uC640 \uBE44\uAD50\uD574 \uADF8 \uB3D9\uC791\uC744\
  \ \uAC80\uC99D\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB9CC\uB4DC\uB294 \uAC83\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB7EC\uD55C \uC2E4\uCC9C\uC740 \uC178 \uC2A4\
  \uD06C\uB9BD\uD2B8\uAC00 \uC758\uB3C4\uD55C \uB300\uB85C \uC791\uB3D9\uD558\uB294\
  \uC9C0 \uD655\uC778\uD558\uACE0, \uC624\uB958\uB97C \uC870\uAE30\uC5D0 \uD3EC\uCC29\
  \uD558\uBA70, \uC720\uC9C0 \uAD00\uB9AC\uB97C \uB354 \uC27D\uAC8C \uB9CC\uB4E4\uAE30\
  \ \uB54C\uBB38\uC5D0\u2026"
lastmod: 2024-02-18 23:09:06.886832
model: gpt-4-0125-preview
summary: "Fish Shell\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uC790\uB3D9\uC73C\uB85C \uC2E4\uD589\uD558\
  \uC5EC \uC608\uC0C1 \uACB0\uACFC\uC640 \uBE44\uAD50\uD574 \uADF8 \uB3D9\uC791\uC744\
  \ \uAC80\uC99D\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB9CC\uB4DC\uB294 \uAC83\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB7EC\uD55C \uC2E4\uCC9C\uC740 \uC178 \uC2A4\
  \uD06C\uB9BD\uD2B8\uAC00 \uC758\uB3C4\uD55C \uB300\uB85C \uC791\uB3D9\uD558\uB294\
  \uC9C0 \uD655\uC778\uD558\uACE0, \uC624\uB958\uB97C \uC870\uAE30\uC5D0 \uD3EC\uCC29\
  \uD558\uBA70, \uC720\uC9C0 \uAD00\uB9AC\uB97C \uB354 \uC27D\uAC8C \uB9CC\uB4E4\uAE30\
  \ \uB54C\uBB38\uC5D0\u2026"
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Fish Shell에서 테스트를 작성한다는 것은 코드를 자동으로 실행하여 예상 결과와 비교해 그 동작을 검증하는 스크립트를 만드는 것을 말합니다. 이러한 실천은 셸 스크립트가 의도한 대로 작동하는지 확인하고, 오류를 조기에 포착하며, 유지 관리를 더 쉽게 만들기 때문에 중요합니다.

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
