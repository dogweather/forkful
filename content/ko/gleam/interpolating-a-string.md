---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

스트링 인터폴레이션은 변수나 표현식을 문자열 안에 직접 삽입하는 것입니다. 이를 통해 개발자들은 문자열 내용으 유동적으로 변경하며, 코드의 가독성도 높일 수 있습니다.

## 실행 방법:

```Gleam
let name = "송혜교"
let greeting = "안녕하세요, ${name}님!"
```
위 코드를 실행하면 결과는 다음과 같습니다:
```Gleam
"안녕하세요, 송혜교님!"
```

## 디테일:

스트링 인터폴레이션은 원래 PerL Language에서 처음 도입되었으며, 편리함과 가독성 때문에 많은 언어로 확산되었습니다. Gleam에서는 `$` 기호를 사용해 변수나 표현식을 문자열에 삽입할 수 있습니다. 

Gleam에는 Python의 f-string방식이나 C#의 string.Format 방식과 같은 대체 방법이 마련되어 있습니다. 그러나 이들 대체 방법들은 보다 복잡하며, 경우에 따라 보다 많은 컴퓨팅 리소스를 필요로 합니다.

스트링 인터폴레이션은 단순히 변수를 문자열로 대체하는 것이 아닙니다. 구현 상세 측면에서, 컴파일러는 문자열과 변수를 설명하는 문장의 복잡한 템플릿을 구성해야 합니다. 이는 문자열 병합보다는 고비용의 작업이지만, 효율적인 인터폴레이션 구현으로 인해 극복할 수 있습니다.

## 참고 자료:

- [Gleam Documentation](https://gleam.run/docs/)
- [String interpolation on Wikipedia](https://en.wikipedia.org/wiki/String_interpolation)
- [Comparison of string interpolation methods in various languages](https://en.wikibooks.org/wiki/Comparison_of_programming_languages_(string_functions)#Interpolation)