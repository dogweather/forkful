---
title:    "Ruby: 문자열을 소문자로 변환하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

Ruby 프로그래밍을 하다보면 종종 문자열을 입력 받아서 소문자로 변환해야 할 때가 있습니다. 이런 경우를 위해 문자열을 소문자로 변환하는 방법을 배우는 것이 매우 중요합니다.

# 어떻게

Ruby에서 문자열을 소문자로 변환하는 방법은 간단합니다. 다음과 같이 `downcase` 메소드를 이용하면 됩니다.

```Ruby
string = "HELLO, WORLD"
puts string.downcase
```

출력 결과는 `hello, world`가 됩니다.


# 깊게 파헤치기

`downcase` 메소드는 문자열 내 모든 알파벳을 소문자로 변환합니다. 예를 들어, `HELLO123`은 `hello123`으로 변환됩니다. 또한, 한글의 경우에도 올바르게 변환해줍니다.

하지만 한 가지 주의할 점이 있습니다. 문자열 내 숫자나 특수 문자는 그대로 유지됩니다. 예를 들어 `THESE ARE NUMBERS: 123`은 `these are numbers: 123`으로 변환되지 않으며 그대로 `THESE ARE NUMBERS: 123`으로 출력됩니다.

# 참고 자료

- [Ruby String Methods](https://www.rubyguides.com/2019/05/ruby-string-methods/)
- [Ruby Official Documentation - String#downcase](https://ruby-doc.org/core-2.6/String.html#method-i-downcase)
- [Ruby String Interpolation](https://www.rubyguides.com/2016/04/ruby-string-interpolation/)
- [Ruby on Rails Tutorial: Learn Ruby on Rails](https://www.railstutorial.org/book)