---
title:                "문자열을 소문자로 변환하기"
html_title:           "Ruby: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것이 중요한 이유는 다음과 같습니다:
1. 사용자의 입력을 일관된 형식으로 유지하기 위해 - 예를 들어, 사용자의 이름을 저장할 때 모두 소문자로 저장하면 나중에 검색하기 쉬워집니다.
2. 문자열을 비교할 때 대소문자를 무시하기 위해 - 대소문자를 무시하면 더 정확한 비교를 할 수 있습니다.

## How To
문자열을 소문자로 변환하는 방법은 간단합니다. `downcase` 메소드를 사용하면 됩니다. 아래 예제를 살펴보세요.

```Ruby
name = "Ruby"
puts name.downcase
```

위 코드를 실행하면 `ruby`라는 출력이 나옵니다.

## Deep Dive
실제로 `downcase` 메소드가 어떻게 동작하는지 깊게 알아보겠습니다. `downcase` 메소드는 문자열의 모든 문자를 소문자로 바꿔주는 역할을 합니다. 이를 위해 ASCII 코드 값을 사용하며, 대문자 알파벳의 코드 값과 소문자 알파벳의 코드 값 사이에는 고정된 차이가 있습니다. 이런 방식으로 `downcase` 메소드는 대문자를 소문자로 변환합니다.

또한, 한글과 같은 다른 언어의 경우에도 `downcase` 메소드를 사용하여 소문자로 변환할 수 있습니다. 이를 위해서는 인코딩에 따라 적절한 변환 방식을 사용해야 합니다. Python과 같은 다른 언어에서도 마찬가지 방식으로 대소문자를 변환하는 메소드들이 제공됩니다.

## See Also
- [Ruby Official Documentation](https://www.ruby-lang.org/ko/)
- [ASCII Codes](https://ko.wikipedia.org/wiki/ASCII)