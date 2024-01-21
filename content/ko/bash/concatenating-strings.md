---
title:                "문자열 연결하기"
date:                  2024-01-20T17:34:03.421987-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

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