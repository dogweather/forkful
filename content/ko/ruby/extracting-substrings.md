---
title:                "하위 문자열 추출"
html_title:           "Ruby: 하위 문자열 추출"
simple_title:         "하위 문자열 추출"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

서브스트링 추출을 왜 해야 할까요? 모든 프로그래머들은 문자열을 다루는 작업을 하게 됩니다. 예를 들어, 우리는 사용자에게 좋은 경험을 제공하기 위해 텍스트를 검증하거나 정제하는 경우가 많습니다. 서브스트링을 추출하는 것은 이런 작업에서 매우 유용하며, 우리의 코드를 보다 간결하고 효율적으로 만드는 데 도움이 됩니다.

## 추출하는 방법

기본적인 문자열 추출 방법은 다음과 같습니다.

```Ruby
string = "Hello, world!"
substring = string[2..5]
puts substring
```

위의 코드를 실행하면 콘솔에 `llo`가 출력됩니다. `string[2..5]`는 `string`에서 인덱스 2부터 5까지의 문자열만을 추출하는 것을 의미합니다. 이와 같은 방식으로 우리는 원하는 위치의 문자열을 추출할 수 있습니다.

또한 정규표현식을 사용하여 더 복잡한 문자열 추출을 수행할 수도 있습니다.

```Ruby
string = "The quick brown fox jumps over the lazy dog"
substring = string[/[aeiou]/]
puts substring
```

위의 코드를 실행하면 `e`가 출력됩니다. 정규표현식 `[aeiou]`는 모음을 포함하는 첫 번째 문자를 추출하는 것을 의미합니다. 이 외에도 정규표현식을 이용해 더욱 다양한 문자열 추출을 할 수 있습니다.

## 깊게 파고들기

문자열 추출을 깊게 이해하려면 문자열 타입의 메소드를 학습해야 합니다. Ruby에서는 `String` 클래스에 다양한 메소드가 있으며, 이를 이용하여 여러 가지 방법으로 문자열을 추출할 수 있습니다. 예를 들어, `slice`, `match`, `scan` 등의 메소드를 이용하여 정규표현식을 사용하거나, `byteslice`를 이용하여 특정 바이트를 추출할 수 있습니다.

## 다른 링크

- [Ruby 문자열 추출 관련 문서](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [정규표현식 문서](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Ruby 문자열 메소드 전체 목록](https://ruby-doc.org/core-2.7.1/String.html#method-i-methods)