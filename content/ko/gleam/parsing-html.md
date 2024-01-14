---
title:                "Gleam: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
HTML 파싱을 할 이유는 무엇일까요? 간단한 이유는 HTML이 웹 페이지의 기본 언어이기 때문입니다. 따라서 웹 개발에서 HTML 코드를 읽고 처리하는 것은 필수적인 작업입니다.

## 어떻게 하나요?
Gleam 언어는 HTML 파싱에 유용한 기능을 제공합니다. 아래는 HTML 파싱 기능을 이용해 웹 페이지에서 제목을 추출하는 예시 코드입니다.
```Gleam
// 필요한 모듈 임포트
import gleam/html

// HTML 코드를 읽고 파싱합니다.
// 여기서는 예시로 Gleam의 공식 홈페이지를 이용합니다.
let html = """
<html>
  <head>
    <title>Gleam, a typed language for the Erlang VM</title>
  </head>
  <body>
    <h1>Welcome to Gleam!</h1>
    <p>Gleam is 또 다른 하나의 문장 예시입니다.</p>
  </body>
</html>
"""

// HTML 코드의 제목을 추출합니다.
let title = html |> gleam/html.parse |> gleam/html.find_title

// 결과 출력
title // Gleam, a typed language for the Erlang VM
```

## 깊은 곳까지 들어가보기
위의 예시에서는 Gleam의 `html` 모듈을 이용해 간단하게 HTML을 파싱하는 방법을 보여주었습니다. 하지만 Gleam은 `html` 모듈 외에도 `parse` 함수와 `find_title` 함수를 이용해 더 다양한 방식으로 HTML을 처리할 수 있습니다. 또한, Gleam의 강력한 타입 시스템을 이용해 HTML 코드의 오류를 사전에 방지할 수 있습니다.

## 더 알아보기
"## 더 알아보기"
- [Gleam 공식 홈페이지](https://gleam.run/)
- [Gleam의 HTML 파싱 예제](https://gist.github.com/istepanov/c1d2de76c6e463ab757fcba58781dd14)
- [Gleam의 `html` 모듈 문서](https://gleam.run/modules/html.html)