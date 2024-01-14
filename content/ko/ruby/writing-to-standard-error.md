---
title:                "Ruby: 표준 에러에 쓰는 방법"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

표준 오류에 쓰기에 참여하는 이유는 코드 디버깅과 에러 관리를 더욱 쉽고 효율적으로 하기 위해서입니다. 표준 오류는 프로그래밍에서 발생하는 에러 메시지들이 모여있는 곳이며, 이를 활용하면 프로그램의 동작을 더 잘 이해할 수 있습니다.

## 어떻게

코드에서 표준 오류에 쓰는 방법은 간단합니다. `puts` 메소드 대신 `warn` 메소드를 사용하여 코드를 작성하면 됩니다. 다음은 간단한 예제 코드와 그에 따른 출력 결과입니다.

```Ruby
# 예제 코드
def divide(num1, num2)
  if num2 == 0
    warn "Cannot divide by zero"
  else
    puts "Result: #{num1 / num2}"
  end
end

# 호출
divide(10, 2)
divide(10, 0)
```

```Shell
# 출력 결과
Result: 5
Cannot divide by zero
```

## 깊이 있는 탐구

표준 오류에 쓰는 것은 간단하지만, 실제로는 많은 기능을 가지고 있습니다. 예를 들어, `warn` 메소드는 에러 메시지 뿐만 아니라 경고 메시지도 출력할 수 있으며, `STDERR.puts`를 사용하면 한 줄씩 메시지를 출력할 수 있습니다. 또한 `STDERR.reopen`을 이용하여 표준 오류를 다른 파일로 바꿀 수도 있습니다.

# 참고

- [Ruby 문서: Kernel 모듈](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-warn)
- [슈퍼에밈의 블로그: 모르면 죽는다?! Ruby 슈피처들의 10가지 "음모"에 대해서](https://blog.superepsilon.com/2014/06/20/consipiracy_ruby_10_things_for_survive/)
- [마이크로소프트 개발자 블로그: STDOUT, STDERR, and sdtin (정복하기)](https://devblogs.microsoft.com/commandline/understanding-stdout-stderr-and-stdin/)