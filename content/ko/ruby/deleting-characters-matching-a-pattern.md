---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Ruby: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

패턴에 해당하는 문자를 삭제하는 것은 일반적으로 코드를 한 줄로 간략하게 만드는 데 사용됩니다. 이는 코드를 더 읽기 쉽게 만들고, 반복적인 작업을 줄이며, 프로그램의 속도를 높이는 데 도움이 됩니다.

## 어떻게?

다음은 간단한 예제 코드입니다:

```ruby
# 문자열에서 모음 제거하기
str = "안녕하세요"
puts str.gsub(/[aeiou]/, "") # '안녕'
```

여기서 ```gsub``` 함수가 사용되었습니다. 이는 지정된 패턴에 해당하는 모든 문자를 두 번째 인자로 대체한 결과를 반환합니다. 위의 예시에서는 ```[aeiou]``` 에 해당하는 모음을 빈 문자열로 대체하여 모음을 삭제하는 동작을 수행합니다.

더 많은 예제 코드를 살펴보면:

```ruby
# 숫자 제거하기
str = "Hello 123"
puts str.delete("^a-z") # 'Hello'

# 특정 문자 제거하기
str = "Hey there, how are you?"
puts str.tr("aeiou", "") # 'Hy thr, hw r y?'
```

```delete``` 함수는 지정된 문자열에 포함되는 모든 문자를 제거하고, ```tr``` 함수는 첫 번째 인자로 지정된 문자들을 두 번째 인자로 대체한 결과를 반환합니다.

## 깊이 빠져보기

문자 삭제 작업은 다양한 방법으로 구현될 수 있습니다. 예를 들어, 위의 예시 코드에서 사용된 ```gsub```, ```delete```, ```tr``` 외에도 ```scan```, ```match``` 함수를 이용해 문자열에서 해당 패턴을 검색할 수 있습니다.

또한 정규식 패턴을 사용하는 것 외에도, 문자열에서 일치하는 패턴을 검색하고 삭제하는 다른 방법들이 있습니다. 예를 들어, Ruby의 ```String``` 클래스에는 ```sub``` 함수가 있는데, 이는 첫 번째 발견된 문자열만 대체하는 함수입니다.

## 관련 링크

- [Ruby Documentation on String](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby Regular Expression Cheat Sheet](https://www.ruby-lang.org/en/documentation/quickstart/4/)
- [Ruby Regex Tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)