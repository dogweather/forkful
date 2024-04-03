---
date: 2024-01-26 03:44:50.854754-07:00
description: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uB370\uC774\uD130\uB97C \uB2E8\
  \uC21C\uD654\uD558\uAC70\uB098 \uD2B9\uC815 \uD615\uC2DD\uC5D0 \uB9DE\uCD94\uAE30\
  \ \uC704\uD574 \uC18C\uC218\uC810 \uC790\uB9AC\uB97C \uC798\uB77C\uB0B4\uB294 \uAC83\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC0AC\
  \uC6A9\uC790 \uCE5C\uD654\uC801\uC778 \uB514\uC2A4\uD50C\uB808\uC774, \uD6A8\uC728\
  \uC801\uC778 \uC800\uC7A5 \uACF5\uAC04 \uC0AC\uC6A9 \uB610\uB294 \uC18C\uC218\uC810\
  \ \uC815\uBC00\uB3C4\uAC00 \uC911\uC694\uD558\uC9C0 \uC54A\uC744 \uB54C \uC774\uB97C\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.845110-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uB370\uC774\uD130\uB97C \uB2E8\uC21C\
  \uD654\uD558\uAC70\uB098 \uD2B9\uC815 \uD615\uC2DD\uC5D0 \uB9DE\uCD94\uAE30 \uC704\
  \uD574 \uC18C\uC218\uC810 \uC790\uB9AC\uB97C \uC798\uB77C\uB0B4\uB294 \uAC83\uC744\
  \ \uB9D0\uD569\uB2C8\uB2E4."
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
