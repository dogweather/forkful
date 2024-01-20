---
title:                "문자열 대문자화"
html_title:           "Fish Shell: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 대문자화란 모든 문자를 대문자로 변환하는 것을 의미합니다. 프로그래머들은 가독성 향상을 위해 이 기능을 사용하곤 합니다.

## 어떻게 사용하나요

```Fish Shell
function ucfirst -d "대문자로 바꾸기"
    echo (string upper $argv)
end
```

위 코드를 실행하면, 문자열을 대문자로 변환하는 `ucfirst` 함수를 생성합니다. 이 함수를 사용해 다음과 같이 문자열을 대문자로 변환할 수 있습니다:

```Fish Shell
ucfirst "hello world"
```

출력:

```Fish Shell
HELLO WORLD
```

## 깊이 있는 탐구

1. **역사적 배경**: 대문자화는 타이핑머신 시대부터 있었습니다. 이를 통해 문서 내용을 강조하거나 부제목을 만드는 등 다양한 이유로 사용되었습니다.

2. **대안들**: Ain't 없으면 는 소문자 문자열을 대문자로 변환하는 함수 말고도, `string upper` 함수를 직접 사용할 수도 있습니다.

3. **구현 세부사항**: 이 함수는 `string upper` 내장 함수를 사용하여 작동합니다. Fish Shell에서 문자열 관련 작업을 위해 제공된다.

## 참고 자료

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell 스크립팅 튜토리얼](https://fishshell.com/docs/current/tutorial.html)