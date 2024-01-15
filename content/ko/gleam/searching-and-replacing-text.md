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

## 왜?

텍스트를 찾고 바꾸는 작업에 관심을 가질 이유에 대해 깊이 이야기해보겠습니다. 이 작업을 수행하는 간단한 방법을 소개하고, 더 깊이 알아보기 위해 마지막으로 깊이 파고들어보겠습니다.

## 어떻게 하나요?

```Gleam replace("hello world", "world", "everyone")```

위의 코드는 "hello world"라는 문자열을 가지고 있고, 그 중 "world"라는 부분을 "everyone"으로 바꾸는 코드입니다. 이렇게 하면 출력값으로 "hello everyone"이 나오게 됩니다. 이렇게 간단하게 코드 한 줄로 텍스트를 찾고 바꾸는 작업을 수행할 수 있습니다.

```Gleam regex_replace("My email is john@gmail.com", "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}", "<EMAIL>")```

위의 코드는 이메일 패턴을 정규식을 이용하여 찾고, "<EMAIL>"이라는 단어로 바꾸는 코드입니다. 이렇게 할 경우 출력값으로 "My email is <EMAIL>"이 나오게 됩니다. 정규식을 사용하면 좀 더 복잡한 패턴을 쉽게 찾고 바꿀 수 있습니다.

## 깊이 파고들기

텍스트를 찾고 바꾸는 작업은 매우 간단하고 유용한 작업입니다. 그러나 정규식을 사용하면 더 다양한 패턴을 찾고 바꾸는 것이 가능하며, 매우 유연한 작업을 할 수 있습니다. 또한, Gleam 언어를 사용하면 다른 프로그래밍 언어보다 더 간결한 코드로 작업을 수행할 수 있으며, 더 많은 기능을 제공합니다.

## See Also

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Gleam을 사용한 간단한 프로젝트 예제](https://github.com/gleam-lang/gleam/tree/master/examples)