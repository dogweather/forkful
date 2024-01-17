---
title:                "디버그 출력 출력"
html_title:           "Kotlin: 디버그 출력 출력"
simple_title:         "디버그 출력 출력"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇이고 왜? 

디버그 출력이란 무엇일까요? 이는 개발자들이 프로그램이 어떻게 동작하는지를 확인할 수 있는 출력 문장이라고 할 수 있습니다. 이는 프로그래밍 과정에서 문제를 일으키는 부분을 찾는 데에도 도움이 되며, 코드가 정상적으로 실행되는지 확인하는 데에도 유용합니다.

## 하는 방법:

```Kotlin
fun main() {
    var num = 10
    println("현재 num의 값은 $num입니다.")
}
```

위의 예시에서는 디버그 출력을 사용하여 ```num``` 변수의 값이 어떻게 변하는지를 확인합니다. 결과 값으로는 "현재 num의 값은 10입니다."라는 문장이 콘솔에 출력됩니다.

## 깊게 들어가기:

디버그 출력은 프로그래밍의 초기 단계부터 사용되어 왔습니다. 이는 디버깅 과정에서 가장 대표적으로 사용하는 방식 중 하나입니다. 그러나 디버그 출력의 문제는 이전에 선언한 변수들의 값을 파악하기 어렵다는 것입니다. 이를 위해 대안으로 콘솔에 출력하는 것 외에도, 디버크 Viewers나 디버그 메시지를 로그 파일에 저장하는 방법도 존재합니다.

## 더 알아보기:

- [Kotlin 설명서 - 디버깅](https://kotlinlang.org/docs/tutorials/command-line.html)
- [네이버 D2 - 디버깅 기법과 로그 지원 라이브러리 소개](https://d2.naver.com/helloworld/10963)
- [Kotlin Dev - 디버깅과 로그 출력](https://www.kotlindev.com/tutorials/debugandlog.html)