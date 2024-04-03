---
date: 2024-01-26 03:44:50.854754-07:00
description: "\uBC29\uBC95: Fish\uC5D0\uC11C, \uC22B\uC790 \uBC18\uC62C\uB9BC\uC740\
  \ `math` \uBA85\uB839\uC5B4\uC5D0 \uC758\uC874\uD569\uB2C8\uB2E4. \uAC00\uC7A5 \uAC00\
  \uAE4C\uC6B4 \uC815\uC218\uB85C \uBC18\uC62C\uB9BC\uD558\uAE30 \uC704\uD574 `math\
  \ -s0`\uC744 \uC0AC\uC6A9\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:55.845110-06:00'
model: gpt-4-0125-preview
summary: "Fish\uC5D0\uC11C, \uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 `math` \uBA85\uB839\
  \uC5B4\uC5D0 \uC758\uC874\uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
Fish에서, 숫자 반올림은 `math` 명령어에 의존합니다. 가장 가까운 정수로 반올림하기 위해 `math -s0`을 사용하세요.

```fish
# 올림
echo (math -s0 "4.7")
# 출력: 5

# 버림
echo (math -s0 "4.3")
# 출력: 4

# 소수 둘째자리까지 반올림
echo (math -s2 "4.5678")
# 출력: 4.57

# 음수 반올림
echo (math -s0 "-2.5")
# 출력: -3
```

## 심층 탐구
역사적으로 숫자 반올림은 더 수동적으로 혹은 외부 도구를 이용해 수행되었으나, Fish와 같은 현대 셸(shell)에서는 내장 유틸리티에 내장되어 있습니다. `math` 명령어를 사용하는 Fish의 접근 방식은 이전 셸에 비해 사물을 단순화합니다. 다른 프로그래밍 환경에서의 대안은 다양하며, Python 같은 언어는 `round()` 함수를 사용하는 반면, Bash는 보다 복잡한 표현이나 `bc` 유틸리티를 필요로 할 수 있습니다. Fish의 반올림 구현은 다른 도구나 언어를 호출하는 대신 셸 환경 내에서 수학을 유지함으로써 스크립팅을 단순화합니다.

## 참고자료
- `math` 명령어에 대한 Fish 문서: https://fishshell.com/docs/current/cmds/math.html
- 부동 소수점 연산에 대한 IEEE 표준 (IEEE 754): https://ieeexplore.ieee.org/document/4610935
