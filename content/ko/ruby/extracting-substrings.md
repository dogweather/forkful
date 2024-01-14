---
title:                "Ruby: 하위 문자열 추출하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜 

파이썬 프로그래밍을 하시는 분들에게는 반드시 이해해야 할 주제가 있습니다. 바로 'substring(부분 문자열) 추출' 입니다. 이 기술은 특정 문자열에서 필요한 부분만을 추출하여 사용할 수 있게 해주는 매우 유용한 기능입니다. 따라서 파이썬 프로그래밍을 할 때는 substring 추출을 반드시 익혀 둘 필요가 있습니다.

## 방법

```Ruby 
str = "안녕하세요, 반가워요!"

puts str[0..2] # => 안녕
puts str[5..7] # => 반가워
puts str[8..10] # =>요!
```

위 코드를 실행해보면 해당 문자열에서 부분 문자열을 추출하는 방법을 직관적으로 이해할 수 있습니다. 문자열 변수 뒤에 `[…]`를 붙이고 그 안에는 추출하고 싶은 부분의 인덱스 값을 넣어주면 됩니다. 인덱스 값은 0부터 시작하며 원하는 범위를 설정해주기 위해서는 `..`를 이용하여 시작과 끝을 표시해주면 됩니다.

## 깊은 곳 까지 

처음에는 문자열에서 간단한 부분을 추출하는 것만으로 충분했지만 더 깊게 들어가보면 더 다양한 활용 방법을 찾을 수 있습니다. 예를 들어, `str` 변수가 여러 줄의 문자열로 이루어진 경우 `str.lines`를 이용하여 문자열 세 줄을 각각 추출할 수 있습니다. 또한 `str.split`을 이용하여 문자열을 공백 기준으로 분리할 수도 있습니다. 이렇게 다양한 방법으로 substring 추출을 활용하면 더 효율적인 코드를 작성할 수 있을 것입니다.

## 알려드리는 것

위에서 소개한 내용 말고도 문자열에서 추출하는 다양한 방법들이 있습니다. 이 중에서 유용한 몇 가지 자료들을 아래 목록에서 확인해보세요.

## 더 자세한 내용은

- [Ruby 문자열 자르기](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [Ruby String 클래스](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby Substring 추출 방법](https://www.rubyguides.com/2019/02/ruby-substring/)
- [Ruby 문자열 다루기](https://mulgun.github.io/2017/03/26/ruby-string.html)

더 많은 정보를 얻고 싶다면 위 링크들을 통해 공부해보세요. 파이썬 프로그래밍을 할 때 substring 추출을 잘 익혀두면 적절한 곳에 적절한 용도로 활용할 수 있게 될 것입니다.