---
title:    "Ruby: 디버그 출력 출력"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜 디버그 출력을 사용하는가?

디버그 출력은 소프트웨어 개발 과정에서 디버깅을 도와주는 중요한 도구입니다. 디버그 출력을 사용하면 코드의 실행 중에 변수 및 객체의 상태를 확인할 수 있으며, 이를 통해 문제를 파악하고 해결할 수 있습니다.

## 어떻게 사용하는가?

```Ruby
num1 = 10
num2 = 20
sum = num1 + num2
puts "num1의 값: #{num1}"
puts "num2의 값: #{num2}"
puts "합계: #{sum}"
```

위의 예시에서는 변수의 값을 확인하기 위해 "puts" 메소드를 사용하여 디버그 출력을 함께 출력하고 있습니다. 이를 통해 각 변수의 값과 합계를 확인할 수 있습니다.

## 더 깊이 파헤쳐보기

디버그 출력을 사용할 때 주의할 점은 너무 많은 출력을 하면 코드의 실행 속도가 느려질 수 있다는 것입니다. 따라서 어떤 부분에서 문제가 발생하고 있는지를 정확히 파악한 후 해당 부분에만 디버그 출력을 추가하는 것이 좋습니다. 또한, 디버그 출력을 사용하면서 코드의 가독성이 떨어질 수 있으므로 필요한 경우 주석을 추가하여 코드를 설명하는 것이 좋습니다.

## 관련 자료

 - [Ruby의 디버그 출력에 관한 블로그 포스트 (영어)](https://www.rubyguides.com/2015/03/ruby-debugging-tips/)
 - [디버그 출력을 활용하는 방법 (한국어)](https://rloveless.tistory.com/14)
 - [Ruby 디버깅을 위한 도구들 (영어)](https://www.sitepoint.com/10-ruby-debugging-tips/)

# 함께 보기

 - [Ruby 디버깅 공식 문서 (한국어)](https://ruby-doc.org/core-2.7.1/doc/debugger.html)
 - [디버그 코드를 작성하는 여러 가지 방법 (영어)](https://robots.thoughtbot.com/debugging-code-tricks-and-tips)