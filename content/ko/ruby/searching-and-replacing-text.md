---
title:                "Ruby: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜  Ruby 프로그래밍을 하느냐? Ruby는 간단하고 읽기 쉬운 문법을 가지고 있어서 개발자들이 코드를 작성하는 데 있어서 큰 편의성을 제공합니다. 또한 Ruby는 다른 언어에 비해 생산성이 뛰어나기 때문에 개발 시간을 단축시킬 수 있습니다.

## 사용 방법
다양한 프로그래밍 언어에서는 특정 텍스트를 찾고 바꾸는 데에 정규식(Regular Expression)을 사용합니다. 하지만 Ruby는 이를 더욱 간단하게 해결할 수 있는 방법을 제공합니다. 다음은 Ruby에서 특정 텍스트를 찾고 바꾸는 방법의 예시입니다.

```Ruby
# 텍스트 찾기와 바꾸기 예시
str = "안녕하세요, 반가워요!"
str.gsub!("반가워요", "반가워요?")
puts str
```

위 예시 코드를 실행하면 "안녕하세요, 반가워요?"라는 결과가 나옵니다. gsub! 메소드는 첫 번째 매개변수로 바꿀 텍스트를, 두 번째 매개변수로 변경할 텍스트를 받습니다. 이를 활용하면 간단하게 특정 텍스트를 찾고 바꿀 수 있습니다.

## 심층 분석
Ruby에서 문자열(string)은 수정할 수 없는(immutable) 객체입니다. 따라서 대부분의 문자열 메소드는 원본 문자열을 수정하지 않고 새로운 문자열을 반환합니다. gsub! 메소드는 예외적으로 원본 문자열을 수정하고 새로운 문자열을 반환하지 않습니다. 이를 주의해서 사용해야 합니다.

또한, gsub 메소드는 단순히 첫 번째 매개변수에 해당하는 텍스트를 모두 찾아서 두 번째 매개변수로 변경하는 것이 아니라, 정규식을 통해 조금 더 복잡한 작업을 할 수 있도록 도와줍니다.

## 참고 자료
- [Ruby gsub, gsub! 메소드](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [정규식(Regular Expression)](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C%EC%8B%9D)