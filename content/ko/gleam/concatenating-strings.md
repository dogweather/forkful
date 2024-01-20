---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

"문자열 연결"은 두 개나 그 이상의 문자열을 하나로 이어붙이는 것입니다. 이 때문에 프로그래머들은 자료 처리와 사용자에게 정보를 제공하는 데 사용하곤 합니다.

## 동작 방법:

Gleam에서의 문자열 연결을 위해 '++' 연산자를 사용합니다. 아래는 간단한 예제 코드입니다:

```gleam
let greeting = "안녕하세요, " ++ "세상아"
io.println(greeting)  // 출력: 안녕하세요, 세상아
```

## 깊이 있는 탐구:

1. 역사적 맥락: 문자열 연결은 컴퓨터 프로그래밍의 초창기부터 존재하였습니다. 처음에는 메모리 절약을 위해 기존 문자열에 새 문자열을 '붙여' 사용하였습니다.

2. 대체 방안: Gleam에는 문자열 형식 지정 기능도 있습니다. 이를 사용하면 다양한 변수 값을 단일 문자열로 결합할 수 있습니다.

```gleam
let name = "세상아"
let greeting = "안녕하세요, " ++ name
io.println(greeting)  // 출력: 안녕하세요, 세상아
```

3. 구현 세부 정보: Gleam의 문자열 연결은 내부적으로 문자열을 결합하는 메모리 복사 연산을 수행합니다. 이로 인해 새로 연결된 문자열이 생성되며, 원래의 문자열은 변경되지 않습니다.

## 참고 자료:

- Gleam 공식 문서: (https://gleam.run/docs/)

- 문자열 관련 기능에 대한 Gleam 튜토리얼: (https://gleam.run/tour/)