---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:46.786599-07:00
description: "Bash\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uB294 \uAC83\
  \uC740 Bash \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uAE30\uB2A5\uC744 \uAC80\uC99D\uD558\
  \uAE30 \uC704\uD574 \uD14C\uC2A4\uD2B8 \uCF00\uC774\uC2A4\uB97C \uC2A4\uD06C\uB9BD\
  \uD305\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uB2E4\uC591\uD55C \uC870\uAC74\uC5D0\uC11C \uC2A4\uD06C\uB9BD\
  \uD2B8\uAC00 \uC608\uC0C1\uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30 \uC704\uD574 \uD14C\uC2A4\uD2B8\uB97C \uC218\uD589\uD558\uC5EC, \uBC30\
  \uD3EC \uC804\uC5D0 \uC624\uB958\uC640 \uBC84\uADF8\uB97C \uC7A1\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.486843-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uB294 \uAC83\
  \uC740 Bash \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uAE30\uB2A5\uC744 \uAC80\uC99D\uD558\
  \uAE30 \uC704\uD574 \uD14C\uC2A4\uD2B8 \uCF00\uC774\uC2A4\uB97C \uC2A4\uD06C\uB9BD\
  \uD305\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uB2E4\uC591\uD55C \uC870\uAC74\uC5D0\uC11C \uC2A4\uD06C\uB9BD\
  \uD2B8\uAC00 \uC608\uC0C1\uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30 \uC704\uD574 \uD14C\uC2A4\uD2B8\uB97C \uC218\uD589\uD558\uC5EC, \uBC30\
  \uD3EC \uC804\uC5D0 \uC624\uB958\uC640 \uBC84\uADF8\uB97C \uC7A1\uC2B5\uB2C8\uB2E4\
  ."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 무엇이며 왜인가?
Bash에서 테스트를 작성하는 것은 Bash 스크립트의 기능을 검증하기 위해 테스트 케이스를 스크립팅하는 것을 포함합니다. 프로그래머는 다양한 조건에서 스크립트가 예상대로 작동하는지 확인하기 위해 테스트를 수행하여, 배포 전에 오류와 버그를 잡습니다.

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
