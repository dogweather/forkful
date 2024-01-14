---
title:                "Ruby: 문자열 대문자로 변경하기"
simple_title:         "문자열 대문자로 변경하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 바꾸는 것이 왜 필요한지 궁금하신가요? 여러분들이 흔히 사용하는 그 로그인 버튼 혹은 회원가입 버튼을 예로 들어볼까요? 모두 첫 글자가 대문자로 시작하는데, 이는 사용자들이 더 쉽게 식별할 수 있도록 하기 위함입니다. 이러한 작은 디테일이 사용자 경험에 큰 영향을 줄 수 있습니다.

## 어떻게

Ruby는 이를 손쉽게 처리할 수 있는 다양한 방법을 제공합니다. 우선, `capitalize` 메소드를 사용하는 방법이 있습니다. 아래 코드를 통해 확인해보세요.

```Ruby
name = "john"
puts name.capitalize # John
```

그런데, 한 번에 모든 단어의 첫 글자를 대문자로 바꾸고 싶다면 `titleize` 메소드를 사용하면 됩니다. 예를 들어, `john smith`를 `John Smith`로 만들고 싶다면 아래와 같이 작성하면 됩니다.

```Ruby
name = "john smith"
puts name.titleize # John Smith
```

## 깊이 있는 정보

단순히 첫 글자를 대문자로 바꾼다는 것만이 `capitalize`의 역할은 아닙니다. 이 메소드는 알파벳이 아닌 언어의 첫 글자도 올바르게 변환할 수 있습니다. 또한, `capitalize`의 경우 두 번째 인자를 추가하여 특정 언어나 문화권에 맞는 대문자 변환을 할 수도 있습니다.

## 참고자료

- Ruby 공식 문서 (한국어): https://ruby-doc.org/core-2.7.1/doc/syntax/methods_rdoc.html#label-String+and+Symbol+Methods
- 루비 도서관: https://ruby-library.com/
- 깃허브 리포지토리: https://github.com/ruby/ruby/tree/ruby_2_7