---
date: 2024-01-20 17:34:03.421987-07:00
description: "\uBB38\uC790\uC5F4 \uACB0\uD569\uC740 \uBB38\uC790\uC5F4\uC744 \uC11C\
  \uB85C \uBD99\uC774\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB97C\
  \ \uD1B5\uD574 \uAC1C\uBC1C\uC790\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uC870\uD569\
  \uD558\uACE0, \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uC131\uD558\uBA70, \uB3D9\uC801\uC73C\
  \uB85C \uCF54\uB4DC\uB97C \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.467259-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uACB0\uD569\uC740 \uBB38\uC790\uC5F4\uC744 \uC11C\uB85C\
  \ \uBD99\uC774\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB97C \uD1B5\
  \uD574 \uAC1C\uBC1C\uC790\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uC870\uD569\uD558\
  \uACE0, \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uC131\uD558\uBA70, \uB3D9\uC801\uC73C\uB85C\
  \ \uCF54\uB4DC\uB97C \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## What & Why? (무엇과 왜?)

문자열 결합은 문자열을 서로 붙이는 것을 의미합니다. 이를 통해 개발자들은 데이터를 조합하고, 메시지를 구성하며, 동적으로 코드를 생성할 수 있습니다.

## How to: (어떻게 하나요?)

```Bash
# 변수 사용하기
greeting="안녕하세요, "
name="철수님!"
welcome_message=$greeting$name
echo $welcome_message # 출력: 안녕하세요, 철수님!

# 문자열 직접 결합하기
echo "프로그래밍 " "재미있어요!" # 출력: 프로그래밍 재미있어요!

# 중괄호로 변수 구분하기
prefix="mega"
suffix="byte"
full_word=${prefix}${suffix}
echo $full_word # 출력: megabyte
```

## Deep Dive (심층 분석)

문자열 결합 기능은 언어가 생겨난 초기부터 존재합니다. Bash와 같은 셸 스크립팅 언어에서는 문자열을 결합하는 방법이 매우 단순합니다. 변수 또는 문자열을 나란히 작성하면 Bash가 자동으로 결합합니다.

JavaScript나 PHP 같은 현대 언어는 문자열 템플릿이나 연산자를 사용하여 보다 복잡한 문자열 결합을 할 수 있지만, Bash는 여전히 간결함을 유지합니다.

대안으로 `paste` 명령어 같은 외부 프로그램을 이용해 파일에서 문자열을 결합할 수도 있습니다. 그러나 일반적인 스크립트 작업에서는 Bash 내장 기능이 유용합니다. 문자열 연산이 많이 필요하지 않은 작업을 위해 간단하고 효율적인 선택입니다.

## See Also (참고 자료)

- Bash manual: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- Stack Overflow: [https://stackoverflow.com/](https://stackoverflow.com/) - 문자열 결합에 대한 다양한 토론 및 예제 검색 가능
