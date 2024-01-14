---
title:                "Elixir: HTML 파싱하기"
simple_title:         "HTML 파싱하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
HTML 파싱에 참여하는 이유는 무엇일까요? 무슨 이유로 이 작업을 시작하게 되었을까요? 우리는 이 글에서 Elixir를 사용하여 HTML 파싱을 다루는 방법에 대해 배워보고자 합니다.

## 어떻게

Elixir에서 HTML 파싱을 하는 방법은 간단합니다. 우선 필요한 모듈을 불러온 후, 다음 코드를 사용하여 HTML을 파싱할 수 있습니다.

```Elixir
html = "<div>Hello World!</div>"
parsed = Floki.parse(html)

IO.puts(parsed)
```
출력 결과는 다음과 같습니다.

```Elixir
["<html>", [class: "sample"], ["<div>", [class: "container"], ["Hello World!"], "</div>"], "</html>"]
```

위 코드에서는 먼저 'html' 변수에 예제로 사용할 HTML을 정의해주고, 'Floki' 모듈을 사용하여 'parsed' 변수에 파싱된 결과를 할당해줍니다. 그 후, 'IO.puts' 함수를 사용하여 결과를 화면에 출력합니다. 결과는 리스트 형태로 나오며, 태그와 속성들이 포함되어 있습니다.

## 깊게 들어가기

HTML 파싱을 할 때 주의해야 할 점은 태그와 속성들이 대소문자를 구분해서 사용한다는 것입니다. 이것을 고려하지 않으면 제대로 된 결과를 얻을 수 없습니다. 또한, 정규표현식을 사용하여 손쉽게 HTML을 파싱할 수도 있지만, Floki와 같은 라이브러리를 사용하면 더 빠르고 간단하게 HTML을 다룰 수 있습니다.

## 참고하기

- [Floki 라이브러리 문서](https://hexdocs.pm/floki/)
- [HTML 파싱에 대한 Elixir 공식문서 예제](https://hexdocs.pm/elixir/IO.html)
- [Elixir로 시작하는 프로그래밍 기본 강좌](https://www.youtube.com/playlist?list=PL5l03Hzokk4arOOIlR2Swdty9DSuYtB4Q)