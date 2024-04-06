---
date: 2024-01-20 17:44:59.628206-07:00
description: "How to: \uB9AC\uB205\uC2A4\uC640 \uC720\uB2C9\uC2A4\uC5D0\uC11C \uB110\
  \uB9AC \uC0AC\uC6A9\uB418\uB294 Bash\uB294 \uC0AC\uC6A9\uC790\uC640 \uC2DC\uC2A4\
  \uD15C \uAC04 \uB300\uD654\uD615 \uD1B5\uC2E0\uC744 \uC81C\uACF5\uD558\uB294 \uBA85\
  \uB839\uC5B4 \uC778\uD130\uD504\uB9AC\uD130\uC785\uB2C8\uB2E4. 1989\uB144\uC5D0\
  \ Brian Fox\uC5D0 \uC758\uD574 \uAC1C\uBC1C\uB418\uC5C8\uC73C\uBA70, \uC774\uD6C4\
  \ \uBB38\uC790\uC5F4 \uCC98\uB9AC\uC640 \uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\
  \ \uAE30\uB2A5\uC774 \uD5A5\uC0C1\uB418\uC5C8\uC2B5\uB2C8\uB2E4. `${}` \uAD6C\uBB38\
  \uC744 \uC0AC\uC6A9\uD558\uBA74 \uAC04\uD3B8\uD558\uAC8C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.140424-06:00'
model: gpt-4-1106-preview
summary: "\uB9AC\uB205\uC2A4\uC640 \uC720\uB2C9\uC2A4\uC5D0\uC11C \uB110\uB9AC \uC0AC\
  \uC6A9\uB418\uB294 Bash\uB294 \uC0AC\uC6A9\uC790\uC640 \uC2DC\uC2A4\uD15C \uAC04\
  \ \uB300\uD654\uD615 \uD1B5\uC2E0\uC744 \uC81C\uACF5\uD558\uB294 \uBA85\uB839\uC5B4\
  \ \uC778\uD130\uD504\uB9AC\uD130\uC785\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to:
```Bash
# 문자열에서 특정 위치의 서브스트링 추출하기
string="Hello, World!"
echo ${string:7:5}  # World

# 문자열 앞에서부터 특정 문자까지 서브스트링 추출하기
echo ${string%%,*}  # Hello

# 문자열 뒤에서부터 특정 문자까지 서브스트링 추출하기
echo ${string##*, }  # World!
```
출력:
```
World
Hello
World!
```

## Deep Dive
리눅스와 유닉스에서 널리 사용되는 Bash는 사용자와 시스템 간 대화형 통신을 제공하는 명령어 인터프리터입니다. 1989년에 Brian Fox에 의해 개발되었으며, 이후 문자열 처리와 서브스트링 추출 기능이 향상되었습니다. `${}` 구문을 사용하면 간편하게 서브스트링을 추출할 수 있으며, 이는 POSIX 표준의 `expr substr` 명령어와 `awk` 또는 `cut` 등의 대안에 비해 매우 빠른 속도로 실행됩니다. 물론, 간단한 작업에서는 눈에 띄는 차이가 없을 수도 있습니다.

## See Also
- Bash 문자열 조작 가이드: https://www.gnu.org/software/bash/manual/
- 리눅스 명령어 튜토리얼: https://tldp.org/LDP/Bash-Beginners-Guide/html/
- 고급 Bash 스크립팅 가이드: https://tldp.org/LDP/abs/html/
