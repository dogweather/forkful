---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 및 왜?
서브스트링 추출은 문자열에서 특정 부분을 선택해서 가져오는 것을 의미합니다. 이것은 우리가 데이터 분석, 검증 혹은 가공을 위해 특정 정보를 추출하고 싶을 때 필요합니다.

## 어떻게?
Gleam에서는 문자열의 한 부분을 가져오려면 `slice` 함수를 사용합니다. 아래 예시를 보면 `sentence`에서 `hello`만 가져옵니다.

```Gleam
let sentence = "hello, world!"
let hello = slice(0, 5, sentence)

assert hello == Ok("hello")
```

만약 시작 혹은 끝 인덱스가 문자열의 범위를 넘어선다면, `Error`를 반환하게 됩니다.

```Gleam
let out_of_bounds = slice(0, 15, sentence)

assert out_of_bounds == Error(Nil)
```

## 깊이 들어가서
서브스트링 추출은 문제의 해당 부분 해결하는데 매우 중요합니다. 이 기술은 덩어리가 큰 데이터에서 고분자 관련 파트만 골라내거나, 사용자가 입력한 이메일 주소에서 도메인을 분리하는 등 다양한 상황에서 사용됩니다.

Gleam의 `slice` 함수는 함수형 언어 경험과 같은 안전한 접근을 제공합니다. 명시적인 `Error` 반환은 인덱스가 잘못되었을 때 요구사항에 따라 개발자가 어떻게 처리해야 할 지 명확하게 알려줍니다.

먼저, 낮은 수준의 문자 검색 (char-counting)를 통해 문자열을 반복하고, 해당하는 유니코드 스칼라 값을 찾습니다. 이 값은 시작 인덱스로, 끝 인덱스를 찾으려면 인덱스를 계속 늘려 나가야 합니다.
이 간단한 절차를 통해 우리는 문자열에 안전하게 접근할 수 있습니다. 

## 참고
문자열에서 서브스트링을 추출하는 것 외에도, Gleam에서 문자열을 다루는 방법은 여러가지입니다. Gleam의 [공식 문서](https://gleam.run/book/tour/strings.html)에서 더 많이 배워보세요. 또한, 문자열 조작에 대해 깊이있게 이해하려면 [이 글](http://www.daemonology.net/blog/2008-06-05-faster-utf8-strlen.html)을 추천드립니다.