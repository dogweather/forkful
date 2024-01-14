---
title:                "Gleam: 정규 표현식 사용하기"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍을 위한 Regular Expressions 용기

## 왜

**Regular Expressions**는 Gleam 프로그래밍에서 유용한 도구입니다. 문자열에 대한 검색, 대체 및 패턴 매칭을 쉽게 구현할 수 있습니다.

## 사용 방법

다음은 Gleam에서 Regular Expressions을 사용하는 예제 코드와 결과입니다.

```Gleam
import gleam/re as re

fn main() {
  let str = "Hello, World!";
  let regex = re.compile("Hello");
  let matches = re.find(regex, str);
  assert matches == ["Hello"];
}
```
위 코드를 실행하면 `["Hello"]`라는 결과가 출력됩니다. 이는 문자열 "Hello, World!"에서 "Hello"라는 패턴이 발견되었기 때문입니다.

## 깊이 파묻기

Regular Expressions에 대해 더 많은 정보를 알아보고 싶다면 다음의 자료를 참고할 수 있습니다:

- [Gleam 공식 문서](https://gleam.run/stdlib/http.html#POST-http_request)에서 Regular Expressions에 대한 자세한 설명을 확인할 수 있습니다.
- [RegexOne](https://regexone.com/)은 Regular Expressions를 배우기에 좋은 사이트입니다. 간단한 예제부터 실무에 활용할 수 있는 예제까지 다양한 학습 자료를 제공합니다.

## 관련 자료

- [Gleam 프로그래밍 언어 소개](https://gleam.run/)는 Gleam 언어의 기본 개념과 특징을 소개하는 공식 사이트입니다.
- [Gleam 슬랙 채널](https://gleam.run/community/chat.html)은 Gleam 사용자들끼리 소통하고 지원을 받을 수 있는 공식 커뮤니티입니다.