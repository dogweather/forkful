---
title:                "문자열 보간"
html_title:           "Ruby: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간은 문자열 내에 다른 변수나 값을 삽입하는 것을 말합니다. 프로그래머들은 이를 사용하여 동적인 문자열을 만들거나 좀 더 가독성 있게 코드를 작성하기 위해 사용합니다.

## 방법:

### 기본적인 보간:

```Ruby
name = "John"
puts "Hello #{name}!" #=> Hello John!
```

### 표현식과 함께 보간:

```Ruby
x = 5
y = 10
puts "The sum of x and y is #{x+y}" #=> The sum of x and y is 15
```

### 조건문과 보간:

```Ruby
age = 25
puts "You are #{age} years old."
puts "You are #{age < 30 ? "still young": "getting older"}." #=> You are 25 years old.
                                                                  You are still young.
```

## 깊게 들어가보기:

일반적으로 보간은 문자열 내에 변수나 값을 삽입하는것을 말하지만, 루비에서는 보간이 더 다양한 기능들을 포함하고 있습니다. 보간은 또한 다른 여러가지 표현식과 결합하여 사용될 수도 있습니다. 예를 들어, 조건문과 함께 사용하여 더 복잡한 문자열을 만들 수 있습니다.

또한 이전 버전의 루비에서는 ```#{}``` 대신에 ```%{}```을 보간에 사용할 수 있었지만, 현재는 거의 사용되지 않는 방식입니다.

## 관련 자료:

- [루비 공식 문서 - 문자열 보간](https://ruby-doc.org/core-2.7.2/doc/syntax/literals_rdoc.html#label-String+Interpolation)
- [루비뻐이어(mybbyr) - 문자열 보간에 대한 더 자세한 설명](https://mybbyr.gitbooks.io/study-ruby-on-rails/content/chapter02/string-interpolation.html)