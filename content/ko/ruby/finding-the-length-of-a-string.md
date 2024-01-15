---
title:                "문자열 길이 찾기"
html_title:           "Ruby: 문자열 길이 찾기"
simple_title:         "문자열 길이 찾기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 길이를 찾는 것이 왜 중요한지에 대해 궁금하신가요? 이 글에서는 Ruby를 사용해 문자열의 길이를 찾는 방법을 알려드릴 것입니다.

## 어떻게
Ruby에서 문자열의 길이를 찾는 방법은 매우 간단합니다. 먼저, `length` 메소드를 사용하여 문자열의 길이를 확인할 수 있습니다. 아래의 예시를 확인해보세요.

```ruby
string = "안녕하세요"
puts string.length
```
출력 결과는 `5`가 됩니다. 이는 한글 5글자와 같으며, 영어의 경우 문자 하나당 1이 됩니다.

## 깊게 들어가보기
`length` 메소드는 실제로는 `size` 메소드와 같습니다. 이 둘은 동일한 값을 반환하지만, 약간의 차이가 존재합니다. `length`는 문자열의 길이를 반환하며, `size`는 문자열 내의 문자의 개수를 반환합니다. 따라서, 문자열에 숫자나 기호가 포함되어 있을 경우 두 메소드의 결과가 다를 수 있습니다.

또한, Ruby에서는 `unicode_normalize` 메소드를 사용하여 유니코드 정규화를 수행할 수 있습니다. 이는 다른 언어로 작성된 문자열의 경우, 길이를 올바르게 계산하기 위해 필요한 단계입니다. 아래의 예시를 참고하세요.

```ruby
string = "こんにちは"
puts string.length
puts string.unicode_normalize.length
```
두 번째 줄에서 사용된 `unicode_normalize` 메소드를 사용하면, `length` 메소드의 결과가 `length` 메소드보다 1 글자 더 많게 나오게 됩니다. 이는 Ruby에서는 모든 문자를 유니코드 단일 문자로 취급하기 때문입니다.

## 더 학습하기
더 많은 Ruby 관련 정보를 얻고 싶다면 아래의 링크들을 참고해보세요.
- [Ruby 공식 홈페이지](https://www.ruby-lang.org/ko/)
- [Ruby 입문서](https://www.slideshare.net/ihoneymon/ss-46247190)
- [RubyKorea 커뮤니티](https://rubykorea.github.io/)