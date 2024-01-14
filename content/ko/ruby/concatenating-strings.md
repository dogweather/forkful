---
title:                "Ruby: 문자열 합치기"
simple_title:         "문자열 합치기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 이유는 문자열 데이터를 조합하여 더 긴 문자열을 만들어야 할 때 유용하기 때문입니다.

여러 개의 단어를 하나의 문장으로 만들거나 변수와 문자열을 함께 출력해야 할 때, 이 기능을 사용하면 코드를 간결하게 유지할 수 있습니다.

## 사용 방법

```Ruby
puts "Hello " + "world"
# 출력: Hello world
```

여기서 `+` 연산자를 사용하여 `"Hello "`와 `"world"` 두 개의 문자열을 하나의 문자열로 연결합니다.

```Ruby
name = "Ruby"
puts "I love " + name + " programming!"
# 출력: I love Ruby programming!
```

변수와 문자열을 함께 사용할 수도 있습니다. 위 예시에서는 변수 `name`에 저장된 문자열인 `"Ruby"`와 함께 출력하고 싶은 문장을 연결해 출력합니다.

## 깊은 곳 들어가기

문자열을 연결하는 데에는 여러 가지 방법이 있습니다. 위의 예시에서는 `+` 연산자를 사용하였지만, `<<` 연산자나 `concat` 메소드를 사용할 수도 있습니다.

`+` 연산자는 새로운 문자열을 만들기 때문에, 연결할 데이터의 크기가 커지면 성능에 영향을 줄 수 있습니다. 반면 `<<` 연산자나 `concat` 메소드는 기존의 문자열에 데이터를 추가하기 때문에 성능 면에서 이점을 가질 수 있습니다.

하지만, 실제로는 이러한 성능의 차이는 거의 무시할 수준이기 때문에 보다 가독성있고 간단한 코드를 작성하기 위해서는 `+` 연산자를 사용하는 것을 권장합니다.

## 참고

[Ruby 문자열 연산자](https://ruby-doc.org/core-2.6.3/String.html#method-i-2B)

[Ruby 메소드 - concat](https://ruby-doc.org/core-2.6.3/String.html#method-i-concat)

[Ruby 첫걸음 - 문자열](https://rubykoans.com/)