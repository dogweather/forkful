---
title:                "Ruby: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

잉여 디버그 출력을 생성하는 이유 : 디버깅은 프로그래밍 과정에서 필수적인 부분이며, 디버그 출력을 통해 코드의 실행 경로를 이해하고 문제를 해결할 수 있습니다.

### 왜

디버그 출력을 생성하는 이유는 해당 코드의 동작을 이해하기 위해서입니다. 디버그 출력을 통해 코드의 실행 경로를 확인하고 각 단계에서 변수의 값을 추적할 수 있습니다.

```Ruby
num1 = 10
num2 = 20
sum = num1 + num2

puts "num1의 값: #{num1}"
puts "num2의 값: #{num2}"
puts "합계: #{sum}"
```

위의 예시 코드에서는 변수의 값과 합계를 확인하기 위해 디버그 출력을 사용하였습니다.

### 어떻게

디버그 출력은 `puts` 명령어를 사용하여 간단하게 생성할 수 있습니다. `puts`는 정해진 포맷에 따라 객체의 값을 콘솔에 출력하는 명령어로, 디버그 출력에 매우 유용합니다.

```
num1의 값: 10
num2의 값: 20
합계: 30
```

### 딥 다이브

디버그 출력을 사용할 때 주의해야 할 점이 있습니다. 디버그 출력이 너무 많거나 불필요한 정보를 출력할 경우 코드의 가독성이 낮아지고 성능에도 영향을 미칠 수 있습니다. 또한, 디버그 출력을 모두 제거하지 않은 채 코드를 배포할 경우 보안 이슈가 될 수 있으므로 주의해야 합니다.

## 참고 자료

- <https://www.rubyguides.com/2016/05/print-output-ruby/>
- <https://www.ruby-lang.org/en/documentation/quickstart/2/>
- <https://www.tutorialspoint.com/ruby/ruby_variables.htm>

---

## 참고 자료

- <https://www.rubyguides.com/2016/05/print-output-ruby/>
- <https://www.ruby-lang.org/en/documentation/quickstart/2/>
- <https://www.tutorialspoint.com/ruby/ruby_variables.htm>