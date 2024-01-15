---
title:                "문자열 연결"
html_title:           "Ruby: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 결합하는 데에는 다양한 이유가 있습니다. 예를 들어, 여러 개의 문자열을 하나로 합쳐서 보기 쉬운 형식으로 표시하거나, 다른 변수나 데이터를 포함한 동적인 문자열을 만들어야 할 때 등이 있습니다. Ruby에서 문자열을 결합하는 방법에 대해 알아보겠습니다.

## 어떻게

```Ruby
# 단일 인용부호를 사용하면 공백 문자열을 추가할 수 있습니다.
"Fizz" + "Buzz" => "FizzBuzz"

# 변수를 이용하여 동적인 문자열을 생성할 수 있습니다.
num1 = 3
num2 = 5

"#{num1} 곱하기 #{num2}는 #{num1 * num2}입니다." => "3 곱하기 5는 15입니다."
```

## 깊이 들어가기

문자열을 결합하는 방법에는 여러 가지가 있습니다. 가장 간단한 방법은 단순히 `+` 연산자를 사용하는 것입니다. 이외에도 `<<`, `+=` 등의 방법도 있습니다. 따라서, 어떤 방법이 가장 효율적인지, 또는 어떤 상황에서 어떤 방법을 사용하는 것이 더 나은지 등에 대해 더 깊이 학습할 필요가 있습니다. 또한, Ruby에서 문자열을 결합할 때 특수한 문자열 객체가 사용되기 때문에 메모리 사용량 등의 측면에서도 고려해야 합니다.

## 관련 링크

- [Ruby 문자열 결합 방법](https://www.rubyguides.com/2019/02/ruby-string-concatenation/)
- [Ruby 문자열 포맷팅](https://www.pluralsight.com/guides/ruby-string-formatting)
- [Ruby 문자열 객체와 메모리 최적화](https://www.sitepoint.com/ruby-string-optimization-tips/)