---
title:    "Ruby: 문자열의 길이 찾기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것이 유용한 이유는 여러 가지가 있습니다. 예를 들어, 문자열의 길이를 알면 문자열을 작업하는 데 필요한 반복 및 조건문의 횟수를 정확하게 파악할 수 있습니다. 또한 문자열의 길이를 알면 문자열에서 특정 부분을 추출하는 등의 작업을 더 쉽게 할 수 있습니다.

# 방법

문자열의 길이를 찾는 방법은 간단합니다. `length` 메소드를 사용하면 됩니다. 아래는 간단한 예제 코드입니다.

```Ruby
str = "안녕하세요!"
puts str.length
```

위 코드를 실행하면 문자열 "안녕하세요!"의 길이, 즉 6이 출력됩니다. 이처럼 `length` 메소드는 문자열의 길이를 반환해주는 간단하고 유용한 메소드입니다.

# 깊게 들어가기

실제로 `length` 메소드는 어떻게 동작하는 걸까요? 우선, 모든 Ruby 객체는 메소드를 호출할 수 있습니다. 따라서 문자열 역시 `length` 메소드를 호출할 수 있습니다. 그렇다면 어떤 일이 벌어질까요?

사실 Ruby에서는 문자열을 내부적으로 배열로 처리합니다. 즉, 문자열의 각 문자가 배열의 요소가 되는 것입니다. 따라서 `length` 메소드를 호출하면 배열의 크기를 반환하는 것과 같은 방식으로 동작합니다.

# 관련 자료

- [Ruby 문서: String 클래스](https://ruby-doc.com/core-2.5.0/String.html)
- [Ruby 모범 사례: 문자열 다루기](https://github.com/bbatsov/ruby-style-guide#strings)
- [Ruby 문자열 관련 기능 더 알아보기](https://www.rubyguides.com/ruby-tutorial/string/#string-size)