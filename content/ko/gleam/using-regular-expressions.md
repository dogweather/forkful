---
title:                "정규식 사용하기"
html_title:           "Gleam: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식을 사용한다는 것은 간단히 말해, 문자열에서 특정한 패턴을 찾아내는 것입니다. 프로그래머들은 이를 사용하여 데이터 검증, 검색 및 변환 등 다양한 작업을 쉽고 빠르게 처리할 수 있습니다.

## 방법:

```Gleam 
import Regex

Regex.match("Gleam", ~r/[A-Z][a-z]/)
=> True
```

위의 예제에서는 정규 표현식 패턴을 사용하여 Gleam에서 첫 글자가 대문자이고 두 번째 글자가 소문자인 경우를 찾았습니다. 이와 같은 방식으로, 여러분은 다양한 패턴을 사용하여 원하는 결과를 얻을 수 있습니다.

## 심층 분석:

(1) 정규 표현식은 1950년대에 등장한 마카이 파슨 (Ken Thompson)이 먼저 개발했습니다. (2) 다른 대안으로는 문자열 메소드를 사용하는 것이 있지만, 정규 표현식은 더 복잡한 패턴을 쉽게 처리할 수 있습니다. (3) Gleam에서는 *Regex* 모듈을 사용하여 정규 표현식을 쉽게 적용할 수 있습니다.

## 관련 자료:

- [Gleam 공식 문서](https://gleam.run/documentation/)