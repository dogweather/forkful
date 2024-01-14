---
title:                "Ruby: 디버그 출력 출력하기"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜

어떤 이유로 디버그 출력을 출력하는 것이 중요할까요? 디버그 출력은 프로그램을 개발하며 코드의 실행을 추적하고 오류를 잡는데 매우 유용합니다. 디버그 출력은 여러분이 작성한 코드의 실행을 실제로 볼 수 있게 해주며, 문제점을 파악하고 해결할 수 있게 해줍니다.

# 사용 방법

디버그 출력을 하기 전에, 필요한 코드에 `require "pry"`를 추가해주어야 합니다. 그리고 디버그를 출력하고 싶은 부분에서 `binding.pry`를 호출하면 됩니다. 다음은 예제 코드와 그 결과입니다.

```ruby
require "pry"

def sum(x, y)
  binding.pry
  x + y
end

result = sum(5, 10)
puts result
```

위 코드를 실행하면, `binding.pry` 호출 이후 코드가 중지되고, 아래와 같이 디버그 콘솔창이 나타납니다.

```
[1] pry(main)> sum(5, 10)
=> 15
```

이제 디버그 콘솔창에서 여러분이 원하는 변수의 값을 확인하거나, 코드를 수정하고 다시 실행해볼 수 있습니다.

# 깊이 알아보기

위의 예제에서 사용된 `binding`은 바로 현재 코드의 실행 컨텍스트를 나타냅니다. `pry`는 이 컨텍스트의 정보를 사용해 디버그 콘솔창을 제공합니다. 또한 `pry`는 코드 중지 후 조사(inspect) 기능을 제공하여 여러분이 원하는 객체나 변수의 값을 자세히 살펴볼 수 있게 해줍니다.

더 많은 정보를 원하신다면, [pry 공식 문서](https://github.com/pry/pry/wiki)를 참고해보시기 바랍니다.

# 또 다른 보기

- [pry 공식 문서](https://github.com/pry/pry/wiki)
- [Ruby 디버깅 툴 비교](https://www.rubyguides.com/2018/01/debugging-ruby/)
- [Ruby - Debugger 모듈](https://www.tutorialspoint.com/ruby-debugger-module)