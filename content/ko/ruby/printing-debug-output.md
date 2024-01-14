---
title:    "Ruby: 디버그 출력 출력"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜
디버그 출력을 사용하는 이유는 다음과 같습니다:

디버그 출력은 코드 실행 중 발생하는 오류를 파악하고 디버깅하는 유용한 도구입니다. 이를 통해 프로그래머는 어떤 부분에서 코드가 작동하지 않는지 빠르게 파악할 수 있고, 문제를 해결하는 데 더욱 효율적으로 접근할 수 있습니다.

## 방법
디버그 출력을 포함하는 메소드는 `puts`, `p`, `print` 등이 있습니다. 이들 메소드는 모두 코드 실행 중 해당 부분에 대한 출력을 생성합니다. 아래는 각 메소드를 사용하는 예시입니다:

```Ruby
puts "This is a debug output." # => This is a debug output.
p "This is a debug output." # => "This is a debug output."
print "This is a debug output." # => This is a debug output.
```

위 예시에서 볼 수 있듯이 `puts`와 `print`는 문자열을 그대로 출력하고 `p`는 따옴표로 감싸진 문자열을 출력합니다. 이를 통해 `p` 메소드를 사용하면 디버그를 더 쉽게 할 수 있습니다.

## 깊이 파헤치기
디버그 출력 메소드의 장점 중 하나는 여러 변수의 값을 확인할 수 있다는 것입니다. 예를 들어서, 다음과 같이 코드를 작성하고 디버그 출력을 사용할 수 있습니다:

```Ruby
number1 = 3
number2 = 4
puts "number1: #{number1}, number2: #{number2}" # => number1: 3, number2: 4
```

위 예시에서는 `number1`과 `number2`의 값을 출력하고 있습니다. 이를 통해 코드 실행 중 변수의 값이 올바른지 확인할 수 있습니다. 또한 디버그 출력을 활용하면 코드 실행 중 조건문의 결과도 확인할 수 있습니다.

## 참고 자료
- [Ruby Docs](https://ruby-doc.org/)
- [Debugging in Ruby](https://www.rubyguides.com/ruby-tutorial/debugging/)