---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:46.786599-07:00
description: "\uBC29\uBC95: Bash\uC5D0\uB294 \uB0B4\uC7A5\uB41C \uD14C\uC2A4\uD2B8\
  \ \uD504\uB808\uC784\uC6CC\uD06C\uAC00 \uC5C6\uC9C0\uB9CC, \uAC04\uB2E8\uD55C \uD14C\
  \uC2A4\uD2B8 \uD568\uC218\uB97C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uBCF4\uB2E4 \uC815\uAD50\uD55C \uD14C\uC2A4\uD2B8\uB97C \uC704\uD574\uC11C\uB294\
  \ `bats-core`\uC640 \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB3C4\uAD6C\uAC00 \uC778\
  \uAE30 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.486843-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uB294 \uB0B4\uC7A5\uB41C \uD14C\uC2A4\uD2B8 \uD504\uB808\uC784\
  \uC6CC\uD06C\uAC00 \uC5C6\uC9C0\uB9CC, \uAC04\uB2E8\uD55C \uD14C\uC2A4\uD2B8 \uD568\
  \uC218\uB97C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Bash에는 내장된 테스트 프레임워크가 없지만, 간단한 테스트 함수를 작성할 수 있습니다. 보다 정교한 테스트를 위해서는 `bats-core`와 같은 서드파티 도구가 인기 있습니다.

### 순수 Bash에서의 기본 테스트 예제:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"

  if [[ "$result" == "$expected_output" ]]; then
    echo "테스트 통과."
    return 0
  else
    echo "테스트 실패. '$expected_output'을(를) 기대했으나, '$result'을(를) 받음"
    return 1
  fi
}

# 테스트 함수 호출
test_example_function
```
샘플 출력:
```
테스트 통과.
```

### 테스트를 위한 `bats-core` 사용하기:
먼저, `bats-core`를 설치합니다. 이는 보통 패키지 관리자를 통하거나 리포지토리를 클로닝해서 할 수 있습니다.

그다음, 별도의 `.bats` 파일에 테스트를 작성합니다.

```bash
# 파일: example_function.bats

#!/usr/bin/env bats

@test "예제 함수 테스트" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"

  [ "$result" == "$expected_output" ]
}
```
테스트를 실행하기 위해, 단순히 `.bats` 파일을 실행하십시오:
```bash
bats example_function.bats
```
샘플 출력:
```
 ✓ 예제 함수 테스트

1 테스트, 0 실패
```

이 접근법을 통해 개발 워크플로우에 테스트를 쉽게 통합함으로써, Bash 스크립트의 신뢰성과 안정성을 보장할 수 있습니다.
