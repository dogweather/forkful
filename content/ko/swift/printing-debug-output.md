---
title:                "디버그 출력을 프린트하기"
html_title:           "Swift: 디버그 출력을 프린트하기"
simple_title:         "디버그 출력을 프린트하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

나는 말이야, 프로그래밍을 배우면, 디버그 출력(printing debug output)이 언제나 우리의 코드를 디버깅하는 데 도움이 된다고 생각해. 그래서 이번 글에서는 디버그 출력이 무엇이고, 왜 프로그래머들이 이를 하는지 알아볼 거야.

## What & Why?

디버그 출력은 단순히 우리의 코드 중간에 값을 출력하는 것을 의미해. 이렇게 하면 코드를 실행할 때 어떤 값이 사용되는지 따라갈 수 있어. 이렇게 출력된 값을 보면, 코드에서 버그가 발생하는 곳을 더 쉽게 찾을 수 있어.

## How to:

### 예제 1:

``` swift
let num1 = 5
let num2 = 10

print(num1 + num2)
```

### 예제 2:

``` swift
func calculateArea(length: Int, width: Int) -> Int {
  let area = length * width

  print("방의 넓이는 \(area)입니다.")

  return area
}

calculateArea(length: 5, width: 10)
```

## Deep Dive:

디버그 출력은 과거에는 프로그래밍의 주요 방법 중 하나였어. 하지만 이제는 디버거(debugger)와 같은 도구들이 발전하면서 많은 프로그래머들이 디버그 출력을 대체하고 있어. 하지만 여전히 간단한 방법으로 코드를 디버깅하는 데 많은 도움이 되는 경우가 많아.

## See Also:

- [The Basics of Debugging in Swift](https://www.twilio.com/blog/basics-of-debugging-in-swift)
- [Advanced Debugging in Swift](https://sweettutos.com/2020/03/31/advanced-debugging-in-swift-using-lldb/)
끝이라지만... 계속 디버그 출력을 사용하면 코드 작성에 많은 도움이 될 거야!