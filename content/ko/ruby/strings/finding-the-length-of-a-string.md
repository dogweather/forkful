---
date: 2024-01-20 17:48:09.625577-07:00
description: "How to: (\uBC29\uBC95) Ruby\uC5D0\uC11C\uB294 `.length` \uB610\uB294\
  \ `.size` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758\
  \ \uAE38\uC774\uB97C \uC27D\uAC8C \uCC3E\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.978907-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uC11C\uB294 `.length` \uB610\uB294 `.size` \uBA54\uC18C\uB4DC\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uC27D\
  \uAC8C \uCC3E\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (방법)
Ruby에서는 `.length` 또는 `.size` 메소드를 사용하여 문자열의 길이를 쉽게 찾을 수 있습니다.

```ruby
str = "안녕하세요"
puts str.length  # 출력: 5
puts str.size    # 출력: 5
```

## Deep Dive (심층 분석)
문자열의 길이를 파악하는 기능은 초기 프로그래밍 언어 개발 당시부터 존재했습니다. Ruby에서 `length`와 `size` 메소드는 거의 동일한 기능을 수행합니다; 둘 다 문자열의 길이를 반환합니다. 내부적으로, Ruby는 문자열의 각 문자(character)를 세어 길이를 계산합니다.

이 메소드들의 차이점은, 일반적으로는 없지만, 몇몇 경우에 `size`가 컬렉션의 크기 측정에 더 적합하게 느껴질 수 있습니다. 배열이나 해시와 같은 다른 컬렉션 타입에서는 `size`를 더 자주 사용하곤 합니다.

`length` 또는 `size`가 문자의 수를 반환하는 동안, 바이트 수를 찾고 싶을 때는 `bytesize` 메소드를 사용할 수 있습니다. 마찬가지로, 유니코드 문자를 정확히 다루고 싶다면 `chars` 메소드를 쓰고 그 결과 배열의 길이를 찾을 수 있습니다.

```ruby
str = "안녕하세요"
puts str.bytesize  # 출력: 15
puts str.chars.length  # 출력: 5
```

## See Also (관련 자료)
- Ruby 공식 문서: [String#length](https://ruby-doc.org/core-3.1.0/String.html#method-i-length)
- Ruby 공식 문서: [String#size](https://ruby-doc.org/core-3.1.0/String.html#method-i-size)
- Ruby API Dock: [String](https://apidock.com/ruby/String)
