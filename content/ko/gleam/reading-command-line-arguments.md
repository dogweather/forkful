---
title:    "Gleam: ''컴퓨터 프로그래밍에서 명령 줄 인수 읽기''"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

이 기사를 읽는 이유는 단순합니다. 글림(Gleam) 프로그래밍 언어를 사용하여 커맨드 라인 인수를 읽는 방법을 배우고 싶기 때문입니다. 커맨드 라인 인수를 읽는 것은 프로그래머에게 유용한 기술이며 프로그램에 유연성을 제공할 수 있습니다.

## 어떻게

커맨드 라인 인수를 읽는 방법을 배우는 것은 간단합니다. 먼저 구글에서 Gleam을 다운로드하여 설치해야 합니다. 그런 다음 다음과 같이 코드를 작성합니다:

```Gleam
import gleam/arg
main(args) {
    greeting := arg.get(args, 0, "Hello")
    name := arg.get(args, 1, "World")
    print(greeting + ",", name + "!")
}
```

위의 코드를 실행하면 예를 들어 "Hello, World!"라는 출력을 볼 수 있습니다.

## 심층적으로 탐구하기

커맨드 라인 인수를 읽는 다른 방법도 있습니다. 예를 들어, `get_flag` 함수를 사용하여 특정 플래그를 읽을 수도 있습니다. 또는 `get_opt` 함수를 사용하여 선택적 인수를 읽을 수도 있습니다. 더 많은 정보는 [Gleam 공식 문서](https://gleam.run/book/stdlib.html#arg)에서 확인할 수 있습니다.

## 또 보기

- [Gleam 공식 웹사이트](https://gleam.run)
- [Gleam 공식 문서](https://gleam.run/book/)
- [Gleam의 커뮤니티 드라이브](https://community.gleam.run/)