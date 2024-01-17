---
title:                "텍스트 검색 및 교체"
html_title:           "Gleam: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Gleam로 문자열 검색 및 교체하기: 그 이유와 방법

## 무엇 & 왜?
텍스트를 검색하고 교체하는 것이 무엇인지 알아야 할까요? 프로그래머들은 왜 이 작업을 하게 될까요? 간단히 설명하면, 검색과 교체는 텍스트에서 원하는 단어나 구문을 찾아서 다른 단어나 구문으로 바꾸는 작업입니다. 이 작업을 해야 하는 이유는, 텍스트를 수정해야 할 때나 잘못된 단어를 바꿔야 할 때 등 다양한 상황에서 도움이 되기 때문입니다.

## 사용법:
`Gleam.[문자열].replace()` 메서드를 사용하면 간편하게 문자열을 검색하고 교체할 수 있습니다. 예를 들어, "안녕하세요"라는 문자열에서 "년"을 "놈"으로 바꾸고 싶다면, 아래와 같이 코드를 작성합니다.
```Gleam
let hello = "안녕하세요"
let newHello = Gleam.[hello].replace("년", "놈")
```
출력 결과는 `안놈하세요`가 될 것입니다.

## 깊이 파헤치기:
검색과 교체는 매우 오래된 개념이며, 많은 다른 프로그래밍 언어에서도 지원됩니다. Gleam도 예외는 아닙니다. 다른 언어와 달리, Gleam의 `replace()` 메서드는 단어가 일치해야 하지만, 정규표현식을 사용하여 더 복잡한 검색과 교체도 가능합니다. 또한, Gleam에서는 불변성을 위해 문자열을 새로 생성하는 것이 아니라 원래 문자열을 수정하는 방식으로 작동합니다.

## 관련 자료:
검색과 교체에 관련된 자세한 정보를 원한다면, 아래 링크들을 참고해 보세요.
- Gleam 공식 문서: https://gleam.run/documentation/std-lib/String.html#replace
- 정규표현식에 대한 자세한 설명: https://www.regular-expressions.info/