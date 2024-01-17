---
title:                "문자열을 소문자로 변환하기"
html_title:           "Fish Shell: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?
문자열을 소문자로 변환하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 이 작업은 문자열을 비교하거나 검색할 때 대소문자를 구분하지 않고 일관성있게 사용하기 위해 필요합니다. 예를 들어, "HELLO"과 "hello"는 두 문자열이 동일한 것이라고 간주되기 때문에, 대소문자를 구분하지 않고 일치 여부를 확인할 수 있습니다.

# 어떻게 하나요?
```Fish Shell```에서 문자열을 소문자로 변환하는 방법은 간단합니다. 다음과 같이 명령어를 입력하면 됩니다:

```
echo "HELLO" | tr '[A-Z]' '[a-z]'
```

위 명령어를 실행하면 "HELLO"가 "hello"로 변환됩니다.

# 깊이 파헤치기
이 작업은 문자열을 소문자로 변환하고자 하는 유형의 여러가지 방법 중 하나입니다. 다른 옵션으로는 ```awk```와 같은 다른 명령어를 사용하는 것이 있습니다. 또한, 프로그래밍 언어마다 문자열을 소문자로 변환하는 내장 함수가 있을 수도 있습니다.

# 관련 자료
- [마크다운 포맷 가이드](https://guides.github.com/features/mastering-markdown/)
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)