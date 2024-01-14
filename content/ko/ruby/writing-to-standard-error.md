---
title:                "Ruby: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜 ? 
표준 오류를 쓰기 위해 노력하는 이유는 무엇인가요? 

표준 오류는 개발자에게 중요한 디버깅 정보를 제공하기 때문에, 디버깅 과정에서 유용합니다. 만약 코드가 예상대로 작동하지 않는다면, 표준 오류는 해당 문제를 진단하고 해결하는데 도움이 될 수 있습니다.

## 사용 방법
표준 오류를 쓰는 방법은 간단합니다. `STDERR` 객체를 사용해서 오류 메시지를 출력하면 됩니다. 다음과 같은 예제 코드를 살펴보세요:
```Ruby
def divide(x, y)
    if y == 0
        STDERR.puts "나누는 수는 0이 될 수 없습니다."
        return nil
    end
    return x / y
end

puts divide(8, 2)
puts divide(10, 0)
```

위의 코드는 `divide` 함수를 정의하고, 만약 나누는 수로 0이 들어온다면, 표준 오류를 사용하여 오류 메시지를 출력하는 예제입니다. 이 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다:
```
4
나누는 수는 0이 될 수 없습니다.
```
첫 번째 줄에서는 정상적으로 나눗셈 결과가 출력되지만, 두 번째 줄에서는 `nil` 값을 반환하고 오류 메시지가 출력됩니다. 

## 더 깊게 들어가기
표준 오류를 쓰는 또 다른 방법으로 `raise` 키워드를 사용하는 것이 있습니다. `raise`는 오류를 일부로 발생시키는 것으로, 디버깅을 위해 코드에서 에러를 일으킬 때 유용합니다. 다음과 같이 사용할 수 있습니다:
```Ruby
num = 10
if num > 5
    raise "숫자가 너무 큽니다."
end
```
위의 예제에서는 만약 `num` 변수에 할당된 값이 5보다 크면, `"숫자가 너무 큽니다."`라는 오류를 일으킵니다. 이렇게 하면 디버깅 과정에서 어떤 부분에서 문제가 발생하는지 쉽게 찾을 수 있습니다.

## See Also
- [Ruby의 표준 라이브러리 문서:실행 중 오류 발생시키기](https://ruby-doc.org/core-3.0.2/Kernel.html#method-i-raise)
- [Debugging in Ruby](https://www.sitepoint.com/debugging-ruby-101/)