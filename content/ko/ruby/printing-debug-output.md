---
title:                "디버그 출력하기"
html_title:           "Ruby: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?:
디버그 출력이 무엇인지 설명하기 위해서는 먼저 디버깅이 무엇인지 알아야 합니다. 디버깅이란 프로그램이나 코드에 있는 버그를 찾고 고치는 것을 말합니다. 이런 과정에서 디버그 출력은 코드를 실행하는 데서 어떤 변수나 조건의 상태를 확인하고 디버깅에 도움을 줍니다. 프로그래머들은 디버그 출력을 사용하여 코드를 디버깅하고 버그를 찾아 해결할 수 있습니다.

## 사용 방법:
```Ruby
def divide(x, y)
    puts "x = #{x}, y = #{y}" # 디버그 출력
    return x / y # 실제 실행 코드
end

puts divide(6, 2) # 함수 실행과 동시에 디버그 출력 출력
# 출력:
# x = 6, y = 2
# 3
```
위 예시에서는 divide 함수를 정의하고, 이 함수에서 디버그 출력을 이용하여 x와 y의 값을 출력합니다. 그리고 함수를 실행할 때마다 디버그 출력이 함께 출력됩니다. 이렇게 하면 코드 실행 도중 변수나 조건의 값이 어떻게 바뀌는지 확인할 수 있고, 디버깅에 도움이 됩니다. 

## 깊이 파헤치기:
디버그 출력은 프로그래밍에서 오랜 역사를 가지고 있습니다. 디버깅을 위해 사용되는 기술 중의 하나입니다. 다른 알터너티브 방법으로는 디버거를 사용하는 것이 있습니다. 디버거는 실행 중인 코드를 일시 정지하고 변수나 조건의 값을 확인하고 코드를 한 줄씩 실행할 수 있도록 도와줍니다.

디버그 출력을 구현하는 방법은 간단합니다. Ruby에서 제공하는 puts 메서드를 사용하면 됩니다. 추가적으로 변수의 값을 확인할 때는 #{} 안에 변수 이름을 넣어 출력할 수 있습니다.

## 참고 링크:
- [Ruby Documentation](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [Debugging Techniques in Ruby](https://www.rubyguides.com/2019/03/ruby-debugging/)
- [Debugging Tools in Ruby](https://www.rubyguides.com/2019/04/debugging-tools/)