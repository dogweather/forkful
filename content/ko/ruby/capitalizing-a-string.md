---
title:                "Ruby: 문자열 대문자로 변환하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜?
문자열의 첫 글자를 대문자로 바꾸는 것이 왜 중요한지 궁금하신가요? 이번에는 Ruby의 대문자화 함수에 대해 알아보겠습니다!

## 어떻게 하나요?
문자열의 첫 글자를 대문자로 바꾸는 방법은 간단합니다. 바로 `capitalize` 함수를 사용하는 것입니다. 아래 예제 코드를 확인해보세요.
```Ruby
string = "hello world"
puts string.capitalize
```
출력 결과: `Hello world`

## 심층적인 분석
`capitalize` 함수는 문자열의 첫 글자를 대문자로 바꾸는 데에만 사용되는 것이 아니라 전체 문자열 중 첫 번째 단어의 첫 글자만 대문자로 바꾸는데에도 사용됩니다. 또한, 영문 이외의 다른 언어에서도 사용할 수 있습니다. 예를 들어, 영어가 아닌 언어의 경우, 다음과 같은 출력 결과가 나옵니다.
```Ruby
string = "여름"
puts string.capitalize
```
출력 결과: `여름`

## 더 알아보기
`capitalize` 함수는 내부적으로 `gsub` 라는 메소드를 사용하여 첫 글자를 대문자로 바꾸는데, 이를 사용하지 않고 직접 구현하는 방법도 있습니다. 자세한 내용은 아래 링크를 확인해주세요.

## 관련 링크
- [String#capitalize 메소드 문서](https://docs.ruby-lang.org/en/2.6.0/String.html#method-i-capitalize)
- [String#capitalize 메소드 소스 코드](https://github.com/ruby/ruby/blob/master/string.c#L2745)
- [Ruby의 대문자화 함수에 대해 알아보는 글](https://www.justinamon.com/code/capitalize-a-string-in-ruby-using-the-correct-encoding-for-i18n/)