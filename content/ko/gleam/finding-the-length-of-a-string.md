---
title:                "Gleam: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜: "문자열의 길이"를 찾는 것에 참여하는 이유를 설명합니다.

문자열의 길이를 찾는 것은 프로그래밍에서 중요한 기술 중 하나입니다. 문자열의 길이를 알면, 문자열을 다루는 작업을 더욱 쉽게 할 수 있기 때문입니다.

## 어떻게 하나요?

```Gleam
let string = "안녕하세요!"
let length = String.length(string)
```

위의 코드는 "안녕하세요!"라는 문자열의 길이를 찾는 법을 보여줍니다. 이제 `length`에는 6이라는 값이 저장되어 있게 됩니다.

또 다른 예제를 보면, 아래와 같이 변수에 값을 직접 할당해도 문자열의 길이를 찾을 수 있습니다.

```Gleam
let string = "Hello, world!"
let length = String.length(string)
```

위의 코드에서 `length`에는 13이라는 값이 저장되어 있게 됩니다.

## 깊이 파고들기

문자열의 길이를 찾는 것은 매우 간단한 작업처럼 보이지만, 실제로는 조금 더 복잡한 과정이 필요합니다. Gleam에서는 문자열의 길이를 찾을 때 Unicode를 지원하기 때문에, 다양한 문자를 처리하는데 있어서도 더욱 정확한 결과를 얻을 수 있습니다.

또한, 문자열의 길이를 찾는 방법은 언어마다 다를 수 있습니다. 이런 차이점을 깊이 알아보면서 언어의 특징에 대해서도 알아볼 수 있습니다.

## 참고자료

- [Gleam 언어 사이트](https://gleam.run/)
- [Unicode 관련 정보](https://unicode.org/)
- [문자열 관련 함수 문서](https://gleam.run/modules/string.html)