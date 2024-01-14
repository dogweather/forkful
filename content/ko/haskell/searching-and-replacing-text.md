---
title:                "Haskell: 텍스트 검색 및 교체하기"
simple_title:         "텍스트 검색 및 교체하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

문자열을 검색하고 바꾸는 것은 프로그램에서 중요한 작업 중 하나입니다. 이 작업은 특정한 문자열을 찾고, 다른 문자열로 대체하여 제거하거나 변경하는 것을 말합니다. 이것은 코드를 더 효율적이고 강력하게 만들어줍니다.

## 어떻게 하나요

우리는 Haskell의 기본적인 함수들을 사용하여 문자열을 검색하고 바꾸는 방법을 알아볼 것입니다. 먼저, "Data.Text" 라이브러리를 임포트해야 합니다. 그리고 우리는 "replace" 함수를 사용하여 검색한 문자열을 다른 문자열로 대체할 수 있습니다. 예를 들어, "Hello World!" 라는 문자열에서 "World"를 "Universe"로 바꾸고 싶다면 다음과 같이 코딩할 수 있습니다.

```Haskell
import qualified Data.Text as T
T.replace "World" "Universe" "Hello World!"
```
위의 코드를 실행하면 "Hello Universe!" 라는 결과가 나옵니다.

또 다른 유용한 기능은 "replace" 대신 "replaceOnce"를 사용하는 것입니다. 이 함수는 첫 번째 발견된 문자열만 대체합니다. 예를 들어, "Hello World! Hello World!" 라는 문자열에서 "World"를 "Universe"로 바꾸는 것이 아니라 첫 번째 "World"만 "Universe"로 바꾸고 싶다면 다음과 같이 할 수 있습니다.

```Haskell
import qualified Data.Text as T
T.replaceOnce "World" "Universe" "Hello World! Hello World!"
```

다음으로 "stripPrefix" 함수를 사용하여 문자열에서 특정한 패턴을 제거할 수 있습니다. 예를 들어, "Hello World!" 라는 문자열에서 "Hello " 부분을 제거하고 싶다면 다음과 같이 코딩할 수 있습니다.

```Haskell
import qualified Data.Text as T
T.stripPrefix "Hello " "Hello World!"
```
위의 코드를 실행하면 "World!" 라는 결과가 나옵니다.

## 딥 다이브

더욱 깊이 들어가서 "replace" 함수와 "replaceOnce" 함수의 동작 방식에 대해 알아보겠습니다. "replace" 함수는 문자열에서 모든 발견된 패턴을 대체하는 반면, "replaceOnce" 함수는 첫 번째 발견된 패턴만 대체합니다. 이것은 코드에서 유용한 기능을 갖춥니다. 또한 "stripPrefix" 함수를 사용하여 문자열에서 특정한 패턴을 제거할 수 있습니다. 이것은 코드를 더 깔끔하고 간결하게 만들어줍니다.

## 이어보기

- [Data.Text 라이브러리 문서](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskell 문자열 함수](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell 문자열 다루기](https://haskell-explained.gitlab.io/blog/posts/2019/10/21/haskell-string.html)

## 참고

이번 글에서는 Haskell을 사용하여 문자열을 검색하고 대체하는 방법에 대해 알아보았습니다. 이 기능을 사용하면 코드를 더 간결하고 효율적으로 만들 수 있습니다. 다음 링크들을 참고하여 더 많은 이해를 할 수 있도록 노력해보세요!