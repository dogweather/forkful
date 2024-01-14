---
title:    "Elm: 텍스트 검색 및 대체하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

텍스트를 검색하고 교체하는 것이 왜 중요한지에 대한 이유는 무엇일까요? Elm으로 프로그래밍할 때, 특히 작업을 더 쉽게 만들어주는 요령에 대해 알아봅시다.

## 어떻게

검색 및 교체 기능을 활용하려면 ```String``` 라이브러리를 사용해야 합니다. 이 라이브러리는 텍스트 조작을 위한 다양한 함수를 제공합니다. 예를 들어, "Hello world!"라는 문자열에서 "world"를 "Elm"으로 교체하는 코드는 다음과 같이 작성할 수 있습니다:

```Elm
import String exposing (replace)

replace "Hello world!" "world" "Elm"
-- output: "Hello Elm!"
```

이 코드는 "Hello world!"라는 문자열에서 "world"를 "Elm"으로 교체한 뒤 "Hello Elm!"을 출력합니다.

또 다른 예시로, "Elm is awesome!"라는 문자열에서 "awesome"를 "fun"으로 교체하는 코드는 다음과 같이 작성할 수 있습니다:

```Elm
import String exposing (replace)

replace "Elm is awesome!" "awesome" "fun"
-- output: "Elm is fun!"
```

이렇게 하면 "Elm is awesome!"라는 문자열에서 "awesome"를 "fun"으로 교체한 뒤 "Elm is fun!"을 출력합니다.

## 깊은 곳으로

검색 및 교체 기능은 텍스트를 다루는 작업에서 매우 유용합니다. 이를 통해 특정 단어나 구문을 빠르게 찾을 수 있고, 필요한 경우 교체할 수 있습니다. 또한 이 기능을 활용하여 텍스트를 정제하거나 언어 번역 등 다양한 작업을 수행할 수 있습니다.

예를 들어, 검색 및 교체 기능을 활용하여 맞춤법 검사를 자동으로 수행할 수도 있습니다. 특정 단어가 검색되면 해당 단어를 언어 번역 API를 통해 자동으로 교체하거나, 맞춤법 검사를 위해 따로 프로그램을 작성할 필요 없이 검색 기능만으로도 해당 단어를 찾아 수정할 수 있습니다.

## 관련 정보

- [Elm 공식 홈페이지](https://elm-lang.org/)
- [Elm 커뮤니티 포럼](https://discourse.elm-lang.org/)
- [String 라이브러리 문서](https://package.elm-lang.org/packages/elm/core/latest/String)