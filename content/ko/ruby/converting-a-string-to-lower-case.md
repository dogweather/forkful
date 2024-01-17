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

# 무엇 & 왜?

문자열을 소문자로 변환한다는 것은 문자열의 모든 문자를 소문자로 변경하는 것을 의미합니다. 이는 대소문자를 구별하지 않는 검색이나 비교를 하기 위해 프로그래머들이 자주 사용하는 기술입니다.

# 방법:

```Ruby
string = "Hello, WORLD!"
puts string.downcase
```

```Ruby
#Output: "hello, world!" 
```

위의 예제에서처럼 `downcase` 메소드를 사용하여 문자열을 소문자로 변환할 수 있습니다. 해당 메소드는 모든 문자를 소문자로 변경하며, 원본 문자열을 수정하지 않고 새로운 문자열을 반환합니다.

# 깊게 파고들기:

- 역사적 배경: 문자열은 다양한 대소문자 정보를 가지고 있을 수 있기 때문에 대소문자를 구분하지 않는 비교는 프로그래밍에서 매우 중요합니다. 따라서 문자열을 모두 소문자로 변환하는 기술은 오래 전부터 사용되고 있습니다.
- 다른 방법: 알파벳 문자 외에도 숫자, 공백 등 다양한 문자를 다루는 경우 `downcase` 메소드가 맞지 않을 수 있습니다. 이 경우 정규식을 사용하여 문자열을 소문자로 변환하는 방법을 사용할 수 있습니다.
- 구현 세부사항: `downcase` 메소드는 문자열 객체의 내부 코드를 변경하는 것이 아니라, 해당 메소드를 호출할 때마다 문자열을 복사하여 새로운 문자열을 생성합니다. 따라서 원본 문자열이 변경되는 위험 없이 안전하게 사용할 수 있습니다.

# 관련 자료:

- [Ruby 문서 - String 클래스](https://ruby-doc.org/core-2.7.0/String.html)
- [ASCII 코드와 대소문자 비교](https://en.wikipedia.org/wiki/ASCII#Character_set)