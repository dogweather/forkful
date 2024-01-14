---
title:                "Ruby: 스트링 연결하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜 문자열 연결이 중요한가요?

우리의 일상 생활에서 문자열 연결은 매우 중요한 작업입니다. 우리가 사용하는 소프트웨어나 앱에서 메세지를 보내거나 출력할 때, 여러 개의 단어나 문장을 하나로 합치는 일은 빈번하게 발생합니다. 따라서 문자열 연결은 프로그래밍에서 매우 유용하고 필수적인 작업입니다.

## 어떻게 하나요?

Ruby에서 문자열 연결을 하기 위해서는 "+" 기호를 사용합니다. 예를 들어, "Hello"와 "World"라는 두 개의 문자열을 연결하고 싶을 때는 아래와 같이 쓸 수 있습니다.

```Ruby
puts "Hello" + "World"
```

위의 코드는 "HelloWorld"라는 문자열을 출력합니다.

## 깊이 파헤쳐보기

Ruby에서 문자열을 연결할 때는 다른 언어들과는 다르게 "String Interpolation"이라는 기능을 사용할 수 있습니다. 이 기능은 문자열 내부에 변수나 식을 넣어서 사용할 수 있게 해줍니다. 예를 들어, 아래의 코드를 살펴보세요.

```Ruby
name = "John"
puts "Hello, my name is #{name}"
```

위의 코드는 "Hello, my name is John"이라는 문자열을 출력합니다. 우리는 변수 name의 값을 문자열 내부에서 사용했습니다. 이렇듯 "String Interpolation"은 문자열을 더 유연하게 다룰 수 있도록 도와주는 중요한 기능입니다.

# See Also

- [Ruby 문자열 연결 방법](https://www.rubyguides.com/2019/09/ruby-string-concatenation/)
- [Ruby 문자열 보간법](https://blog.appsignal.com/2019/01/24/ruby-magic-string-interpolation.html)
- [Ruby 문자열 연결 실습](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-strings/cheatsheet)