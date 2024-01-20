---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
텍스트 검색 및 대체는 특정 문자열을 찾고 그것을 다른 문자열로 바꾸는 과정입니다. 프로그래머들이 이를 사용하는 이유는 데이터를 정리하고, 수정하며, 정보 통보나 디버깅을 간소화하는 데 도움이 되기 때문입니다.

## 방법:
Elixir에서 문자열 대체는 `String.replace/3` 함수를 이용합니다. 코드 예시와 출력을 살펴봅시다.

```Elixir
sentence = "나는 Elixir를 사랑합니다."
IO.puts String.replace(sentence, "Elixir", "프로그래밍")

# 출력: 나는 프로그래밍를 사랑합니다.
```

위 예시는 "Elixir"라는 문자열을 "프로그래밍"이라는 문자열로 대체하였습니다.

## 깊이 들여다보기
텍스트 검색 및 대체는 프로그래밍의 중요한 부분으로, 언어가 처음 개발될 때부터 존재하였습니다. 역사적 맥락으로 보면, 이 기능은 메모리 최적화 및 데이터 처리의 복잡성을 줄이는 데 도움이 되었습니다. Elixir에서는 `String.replace/3` 함수 외에도 `Regex` 모듈을 사용하여 더 복잡한 패턴의 텍스트를 검색하고 대체할 수 있습니다.

## 참고자료
다음은 추후 참고할 수 있는 관련 소스입니다.
1. Elixir 공식 문서의 문자열 모듈: [링크](https://hexdocs.pm/elixir/String.html)
2. Elixir School의 제어 흐름에 관한 세부 정보: [링크](https://elixirschool.com/en/lessons/basics/control-structures/)

이해를 돕기 위한 충분한 정보를 제공했기를 바랍니다. 행운을 빕니다!