---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자 패턴 일치를 삭제하는 것은 특정 아이템(문자 또는 문자열)을 코드에서 제거하는 프로세스입니다. 이는 중복을 제거하거나 사용자 입력 청소와 같은 경우에 유용합니다.

## 어떻게:

여기에서는 Elixir에서 문자 패턴 일치를 어떻게 삭제하는지 살펴보겠습니다.

```elixir
String.replace("안녕하세요, Elixir!", "안녕하세요", "")
```

위 코드의 출력은 다음과 같습니다:

```elixir
", Elixir!"
```

앞의 예제에서 우리는 "안녕하세요"라는 단어를 빈 문자열("")로 교체했습니다. 이것이 패턴 일치를 삭제하는 방법입니다.

## 깊은 탐색:

삭제 패턴 일치는 프로그래밍에서 오래된 개념입니다. 이는 데이터 정제와 해당되는 정보의 추출을 용이하게 하는 것뿐만 아니라 코드의 가독성을 향상시킵니다.

표준 라이브러리를 사용하여 특정 문자 패턴을 제거할 수 있으며, 정규 표현식 라이브러리를 사용하여 좀 더 복잡한 필요한 경우에도 이를 수행 할 수 있습니다.

Elixir String.replace 함수는 문자열 내에서 특정 문자나 패턴을 찾아 다른 문자열로 대체하는 기본 기능을 제공하므로 이 문제를 처리하기에 적합합니다. 이 함수에는 두 가지 매개변수가 필요합니다: 대체될 문자열과 그것을 대체할 문자열입니다.

## 참고

1. [Elixir 공식 문서: String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
2. [프로그래밍에서 정규 표현식 사용](https://ko.wikipedia.org/wiki/정규_표현식)
3. [Elixir를 사용한 단어 교체 및 삭제](https://stackoverflow.com/questions/42446059/replacing-and-deleting-words-with-elixir)