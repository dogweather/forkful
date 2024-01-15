---
title:                "부분 문자열 추출하기"
html_title:           "Elm: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
반가워! 오늘은 Elm으로 알아보는 substring 추출에 대해서 알아보자. substring 추출은 문자열에서 특정 부분을 추출하는데 유용하게 사용될 수 있어서 많은 프로그래머들이 자주 사용하고 있어. 지금부터 어떻게 하면 substring을 추출할 수 있는지 살펴보자.

## 어떻게 하나요
이번에는 앞서 말한 것처럼 substring 추출을 위한 코드 예제와 그 결과를 ```Elm... ``` 코드 블록 안에 넣어서 살펴보겠어.

### 기본 문자열에서 substring 추출하기
```Elm
let
    string = "반가워, Elm 프로그래밍!"

in
    String.left 2 string -- 결과: "반가워"
```

### 시작 index와 길이 지정하여 추출하기
```Elm
let
    string = "반가워, Elm 프로그래밍!"

in
    String.slice 7 13 string -- 결과: "Elm 프로그래밍"
```

### 인덱스 범위로 추출하기
```Elm
let
    string = "안녕하세요? Elm은 정말 멋진 언어입니다."

in
    String.substring 9 15 string -- 결과: "Elm은 정말"
```

## 딥 다이브
그외에도 ```String.take```와 ```String.drop```을 통해 문자열 앞 뒤로 일부 문자를 추출할 수 있어. 또한 ```String.split```을 사용하면 특정 문자를 기준으로 문자열을 나눌 수도 있어.

## 더 알아보기
이 외에도 Elm 문자열을 다루는 방법에 대해 더 알고 싶다면 아래 링크들을 참고해보자.

- [Elm 문자열 관련 문서](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 문자열 관련 예제](https://guide.elm-lang.org/types/strings.html)
- [Elm 문자열 관련 Q&A](https://discourse.elm-lang.org/t/extract-substring-from-string/208/3)

## 참고
이번 글이 너에게 도움이 되길 바라며, 더 많은 Elm 관련 정보를 찾고 싶다면 아래 링크들을 참고해보자!

- [Elm 공식 홈페이지](https://elm-lang.org/)
- [Elm 커뮤니티 블로그](https://blog.elm-community.org/)
- [Elm 한국 사용자 그룹(Facebook)](https://www.facebook.com/groups/elmkr)