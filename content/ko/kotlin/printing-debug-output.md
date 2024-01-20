---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요한가?

디버그 출력이란 코드가 실행되는 동안 발생하는 변수의 값나 상태 등을 확인하는 방법입니다. 프로그래머가 실시간으로 중간결과를 확인할 수 있어 디버깅과 오류 수정에 매우 유용합니다.

## 어떻게 사용하는가?

Kotlin을 사용한 디버그 출력 예제는 아래에 있습니다.

```Kotlin
fun main() {
    val number = 5
    println("변수 number의 값은 $number 입니다.")
}
```

이 코드의 출력은 아래와 같습니다.

```
변수 number의 값은 5 입니다.
```

## 깊이 보기

디버그 출력은 초기 프로그래밍 세계부터 존재해왔고, 복잡한 프로그램에서 이해하고 수정하는데 크게 도움이 되는 도구입니다. Kotlin에서는 `println` 함수가 가장 일반적인 디버그 출력 방법입니다. 다른 대안으로 `print`나 `printf` 함수도 사용할 수 있으나, 이들은 줄바꿈을 자동으로 추가하지 않습니다.

## 참조하기

다음은 디버그 출력에 대해 자세히 알아볼 수 있는 몇 가지 링크입니다.

- 대표적인 Kotlin 문서인 [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- Kotlin 공식 사이트에서 제공하는 [Kotlin Playground](https://play.kotlinlang.org/)

이들 링크를 통해 디버깅 기법과 문법, Kotlin의 다양한 기능을 학습할 수 있습니다.