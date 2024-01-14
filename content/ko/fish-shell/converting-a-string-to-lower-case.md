---
title:    "Fish Shell: 문자열을 소문자로 변환하기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜

왜 누군가 문자열을 소문자로 변환하는 것에 참여할까요? 이는 문자열에 대해 작업할 때 올바른 형식으로 유지하기 위해 중요합니다. 예를 들어, 사용자의 입력을 받아서 데이터베이스에 저장할 때 소문자 형식으로 변환하는 것이 도움이 될 수 있습니다. 또한, 소문자로 변환하면 대소문자를 구분하지 않는 검색이 가능해지므로 유용합니다.

## 해하는 법

Fish Shell에서 문자열을 소문자로 변환하는 방법은 매우 간단합니다. 먼저 문자열을 변수에 할당한 다음 `string tolower` 함수를 사용하면 됩니다.

```Fish Shell
# 문자열 변수 할당
set str "HELLO WORLD"

# 소문자로 변환
set str (string tolower $str)

# 출력
echo $str

# 결과
hello world
```

또는 한 줄로 표현할 수도 있습니다.

```Fish Shell
# 문자열 할당 및 소문자로 변환 후 출력
echo (string tolower "HELLO WORLD")

# 결과
hello world
```

## 깊게 들어가기

문자열을 소문자로 변환하는 방법은 내부적으로 어떻게 이루어지는지 궁금하신가요? Fish Shell에서는 문자열을 변환할 때 내부적으로 Unicode 표준을 따릅니다. 따라서 모든 언어를 지원하며, 다양한 문자를 올바르게 변환해줍니다.

또한, Fish Shell에서는 다양한 함수를 제공하므로 더 많은 문자열 조작이 가능합니다. 예를 들어, 문자열 중간에 포함된 대문자만 소문자로 변환하고 싶다면 `string tolower -l`을 사용하면 됩니다. 또는 `string tolower -u`를 사용하면 모든 소문자를 대문자로 변환할 수도 있습니다.

## 두루 보기

이외에도 Fish Shell에서는 다양한 문자열 조작에 유용한 함수들을 제공합니다. 관련된 링크들을 알려드리겠습니다.

- [Fish Shell 문자열 함수 문서](https://fishshell.com/docs/current/cmds/string.html)
- [Unicode 표준 문서](https://www.unicode.org/)
- [Fish Shell 공식 홈페이지](https://fishshell.com/)