---
title:    "Ruby: 패턴과 일치하는 문자 삭제하기"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

제목: Ruby 프로그래밍에서 문자열 패턴 매칭 삭제하는 이유

## 왜

Ruby 프로그래밍에서 문자열에서 특정 패턴을 매칭하여 삭제하는 것은 가독성과 성능을 향상시키기 위해서입니다. 예를 들어, 사용자 입력에 들어있는 불필요한 문자를 제거하거나, 데이터베이스에서 데이터를 가져와서 특정 문자열을 제거할 때 유용하게 사용될 수 있습니다.

## 어떻게

```Ruby
# 사용자가 입력한 문자열에서 모든 숫자를 제거하는 예제
input = "Ruby123Programming"
output = input.gsub(/\d/, "") # 숫자를 찾아서 제거
puts output # "RubyProgramming"
```

```Ruby
# 배열에서 특정 패턴과 일치하는 문자열 제거하는 예제
array = ["Apple", "Banana", "Cherry", "Durian"]
array.delete_if { |fruit| fruit.start_with?("B") } # "B"로 시작하는 과일 제거
puts array # ["Apple", "Cherry", "Durian"]
```

## 깊이 있는 내용

Ruby에서 제공하는 String 클래스의 `gsub` 메소드는 정규식을 사용하여 패턴과 일치하는 부분을 다른 문자로 대체하거나 제거할 수 있습니다. 또한, 배열에서 제공하는 `delete_if` 메소드는 블록을 이용하여 특정 패턴과 일치하는 요소를 제거할 수 있습니다. 이와 같은 다양한 기능을 활용하여 문자열 패턴 매칭 삭제를 유연하게 다뤄볼 수 있습니다.

## 참고 자료

- [Ruby 문서](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Ruby 메소드 참고](https://www.rubyguides.com/2019/02/ruby-methods/)
- [정규식 자습서](https://rubular.com/)