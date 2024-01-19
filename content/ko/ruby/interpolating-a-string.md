---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 이게 무엇이고 왜 필요한가요?
문자열 내삽(String Interpolation)이란, 문자열 안에 Ruby 코드를 넣어 그 값을 바로 출력할 수 있는 기법을 일컫습니다. 이를 통해 반복적으로 값을 더하거나 변경하지 않아도 되므로 코드를 깔끔하게 유지할 수 있습니다.

## 어떻게 사용해야 할까요?
```Ruby
name = 'Kim'
puts "Hey #{name}, How are you?"
```
위의 코드를 실행하면, `#{name}` 부분이 'Kim'으로 치환되어 "Hey Kim, How are you?"라는 결과가 출력됩니다.

```Ruby
x = 10
y = 20
puts "Sum of #{x} and #{y} is #{x + y}"
```
이란 코드는 "#{x + y}" 부분이 두 수의 합인 '30'으로 치환되어 "Sum of 10 and 20 is 30"이라는 결과를 출력합니다.

## 깊게 알아보기
문자열 내삽은 Ruby의 역사적인 부분으로, 그 기원은 Perl에서 찾을 수 있습니다. Perl보다 더 강력한 표현력을 가지며, JavaScript의 Template Literals, Python의 F-strings 같은 다른 언어의 비슷한 기능과 비교됩니다.

Ruby에서는 '%' 기호나 '+'처럼 다른 기호를 사용해 문자열을 연결하는 대신, "#{...}" 형태로 보다 간결하게 코드를 작성할 수 있습니다.

문자열 내삽은 실제로 문자열의 `to_s` 메소드를 호출하여 값을 문자열로 변환합니다. 이 점은 숫자나 객체를 문자열에 내삽할 때 특히 유용합니다.

## 참고 자료
1. [Ruby Documentation: String Interpolation](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings)
2. [Ruby Learning](http://rubylearning.com/satishtalim/ruby_string_interpolation.html)
3. [Geeks for Geeks: Ruby String Interpolation](https://www.geeksforgeeks.org/ruby-string-interpolation/)